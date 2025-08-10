(ns me.lomin.sinho.timeout
  "Adaptive Self-Clocking Timeout Guard for CLJ/CLJS (single, non-tunable API).")

#?(:clj (set! *warn-on-reflection* true))

;; ---------- Time (µs) ----------

#?(:clj
   (defn now-us []
     (quot (System/nanoTime) 1000))
   :cljs
   (defn now-us []
     (if (and (exists? js/process)
              (.-hrtime js/process)
              (.-bigint (.-hrtime js/process)))
       ;; Node: process.hrtime.bigint() -> ns BigInt
       (let [bn ((.-bigint (.-hrtime js/process)))]
         (js/Number (/ bn (js/BigInt 1000))))
       ;; Browser: performance.now() (ms double) -> µs Number
       (let [ms (if (exists? js/performance)
                  (js/performance.now)
                  (.now js/Date))]
         (js/Math.floor (* 1000 ms))))))

;; ---------- Constants (no tuning) ----------

(def ^:const ^double Z95 1.64485)
(def ^:const ^double ALPHA 0.05)
(def ^:const ^double SAFETY 2.0)
(def ^:const ^double DECAY 0.98)
(def ^:const ^double BURN-RATIO 0.33)
(def ^:const ^long KMAX (bit-shift-left 1 20))
(def ^:const ^double SAMPLE-MAX-SPACING-MS 10.0) ; time-based ceiling for next sample
(def ^:const ^double TARGET-OVERSHOOT-FRACTION 0.05) ; 5% of timeout
(def ^:const ^double TARGET-OVERSHOOT-CAP-MS 10.0) ; 10ms

;; ---------- Guard Protocol ----------

(defprotocol IGuard
  ;; Immutable fields getters
  (get-deadline-us [this])
  (get-target-overshoot-us [this])
  (get-start-us [this])

  ;; Mutable fields getters
  (get-last-sample-us [this])
  (get-steps-since [this])
  (get-stride-k [this])
  (get-mean-us [this])
  (get-var-us [this])
  (get-decayed-max-us [this])
  (get-timed-out [this])
  (get-detected-at-us [this])
  (get-check-count [this])
  (get-sample-count [this])
  (get-panic-count [this])
  (get-stride-min [this])
  (get-stride-max [this])
  (get-stride-shrink-count [this])
  (get-burn-max [this])

  ;; Mutable fields setters
  (set-last-sample-us! [this val])
  (set-steps-since! [this val])
  (set-stride-k! [this val])
  (set-mean-us! [this val])
  (set-var-us! [this val])
  (set-decayed-max-us! [this val])
  (set-timed-out! [this val])
  (set-detected-at-us! [this val])
  (set-check-count! [this val])
  (set-sample-count! [this val])
  (set-panic-count! [this val])
  (set-stride-min! [this val])
  (set-stride-max! [this val])
  (set-stride-shrink-count! [this val])
  (set-burn-max! [this val]))

;; ---------- Guard Implementation ----------

(deftype Guard
         [^long deadline-us
          ^long target-overshoot-us
          ^long start-us

          ^:unsynchronized-mutable ^long last-sample-us
          ^:unsynchronized-mutable ^long steps-since
          ^:unsynchronized-mutable ^long stride-k

          ^:unsynchronized-mutable ^double mean-us
          ^:unsynchronized-mutable ^double var-us
          ^:unsynchronized-mutable ^double decayed-max-us

          ^:unsynchronized-mutable ^boolean timed-out
          ^:unsynchronized-mutable ^long detected-at-us

          ;; Debug counters/gauges
          ^:unsynchronized-mutable ^long check-count
          ^:unsynchronized-mutable ^long sample-count
          ^:unsynchronized-mutable ^long panic-count
          ^:unsynchronized-mutable ^long stride-min
          ^:unsynchronized-mutable ^long stride-max
          ^:unsynchronized-mutable ^long stride-shrink-count
          ^:unsynchronized-mutable ^double burn-max]

  IGuard
  ;; Immutable field getters
  (get-deadline-us [_] deadline-us)
  (get-target-overshoot-us [_] target-overshoot-us)
  (get-start-us [_] start-us)

  ;; Mutable field getters
  (get-last-sample-us [_] last-sample-us)
  (get-steps-since [_] steps-since)
  (get-stride-k [_] stride-k)
  (get-mean-us [_] mean-us)
  (get-var-us [_] var-us)
  (get-decayed-max-us [_] decayed-max-us)
  (get-timed-out [_] timed-out)
  (get-detected-at-us [_] detected-at-us)
  (get-check-count [_] check-count)
  (get-sample-count [_] sample-count)
  (get-panic-count [_] panic-count)
  (get-stride-min [_] stride-min)
  (get-stride-max [_] stride-max)
  (get-stride-shrink-count [_] stride-shrink-count)
  (get-burn-max [_] burn-max)

  ;; Mutable field setters - with proper type casting for primitives
  (set-last-sample-us! [_ val] (set! last-sample-us (long val)))
  (set-steps-since! [_ val] (set! steps-since (long val)))
  (set-stride-k! [_ val] (set! stride-k (long val)))
  (set-mean-us! [_ val] (set! mean-us (double val)))
  (set-var-us! [_ val] (set! var-us (double val)))
  (set-decayed-max-us! [_ val] (set! decayed-max-us (double val)))
  (set-timed-out! [_ val] (set! timed-out (boolean val)))
  (set-detected-at-us! [_ val] (set! detected-at-us (long val)))
  (set-check-count! [_ val] (set! check-count (long val)))
  (set-sample-count! [_ val] (set! sample-count (long val)))
  (set-panic-count! [_ val] (set! panic-count (long val)))
  (set-stride-min! [_ val] (set! stride-min (long val)))
  (set-stride-max! [_ val] (set! stride-max (long val)))
  (set-stride-shrink-count! [_ val] (set! stride-shrink-count (long val)))
  (set-burn-max! [_ val] (set! burn-max (double val))))

(defn- calc-effective [^Guard g]
  (let [mu (get-mean-us g)
        v (max (get-var-us g) 0.0)
        p95 (+ mu (* Z95 (Math/sqrt v)))]
    (max p95 (get-decayed-max-us g) 1.0)))

(defn- calc-stride [^Guard g remaining-us]
  ;; Base stride from budget
  (let [eff (calc-effective g)
        budget (max (- remaining-us (get-target-overshoot-us g))
                    (get-target-overshoot-us g))
        k-base (long (Math/floor (/ (double budget) (* SAFETY eff))))
        ;; Time-based ceiling: never plan the next sample farther than Tmax in wall-clock.
        tmax-us (* 1000.0 SAMPLE-MAX-SPACING-MS)
        k-time (long (Math/floor (/ tmax-us (* SAFETY eff))))
        k (-> k-base (min k-time) (min KMAX))]
    (max 1 k)))

(defn- timeout?-impl [^Guard g]
  ;; Return true to STOP, false to continue.
  (let [s #?(:clj (long (unchecked-inc (get-steps-since g)))
             :cljs (inc (get-steps-since g)))]
    (set-steps-since! g s)
    (set-check-count! g #?(:clj (unchecked-inc (get-check-count g))
                           :cljs (inc (get-check-count g))))
    (if (< s (get-stride-k g))
      false
      ;; SAMPLE PATH
      (let [now (now-us)
            remaining #?(:clj (unchecked-subtract (get-deadline-us g) now)
                         :cljs (- (get-deadline-us g) now))]
        (set-sample-count! g #?(:clj (unchecked-inc (get-sample-count g))
                                :cljs (inc (get-sample-count g))))
        (if (<= remaining 0)
          (do
            (set-timed-out! g true)
            (when (zero? (get-detected-at-us g))
              (set-detected-at-us! g now))
            true)
          (let [elapsed #?(:clj (unchecked-subtract now (get-last-sample-us g))
                           :cljs (- now (get-last-sample-us g)))
                x (/ (double elapsed) (double s))
                a ALPHA
                mu (get-mean-us g)
                d (- x mu)
                mu' (+ mu (* a d))]
            ;; stats
            (set-mean-us! g mu')
            (let [pv (get-var-us g)]
              (set-var-us! g (min (+ (* (- 1.0 a) pv) (* a d d)) 1.0e18)))
            (let [dm (get-decayed-max-us g)]
              (set-decayed-max-us! g (max x (* DECAY dm))))
            ;; stride / panic
            (let [burn (if (pos? remaining) (/ (double elapsed) (double remaining)) 1.0)
                  _ (when (> burn (get-burn-max g)) (set-burn-max! g burn))
                  panic? (> burn BURN-RATIO)
                  new-k (if panic? 1 (calc-stride g remaining))
                  old-k (get-stride-k g)]
              (when panic?
                (set-panic-count! g #?(:clj (unchecked-inc (get-panic-count g))
                                       :cljs (inc (get-panic-count g)))))
              (when (< new-k old-k)
                (set-stride-shrink-count! g
                                          #?(:clj (unchecked-inc (get-stride-shrink-count g))
                                             :cljs (inc (get-stride-shrink-count g)))))
              (when (> new-k (get-stride-max g)) (set-stride-max! g new-k))
              (when (< new-k (get-stride-min g)) (set-stride-min! g new-k))
              ;; reset window
              (set-last-sample-us! g now)
              (set-steps-since! g 0)
              (set-stride-k! g new-k)
              false)))))))

(defn- debug-map [^Guard g]
  (let [now (now-us)
        deadline (get-deadline-us g)
        overshoot (max 0 #?(:clj (unchecked-subtract now deadline)
                            :cljs (- now deadline)))
        mu (get-mean-us g)
        v2 (max (get-var-us g) 0.0)
        p95 (+ mu (* Z95 (Math/sqrt v2)))]
    {:timeout/timeout? (get-timed-out g)
     :timeout/debug
     {:timeout.debug/start-us (get-start-us g)
      :timeout.debug/now-us now
      :timeout.debug/deadline-us deadline
      :timeout.debug/detected-at-us (get-detected-at-us g)
      :timeout.debug/overshoot-us overshoot
      :timeout.debug/target-overshoot-us (get-target-overshoot-us g)

      :timeout.debug/check-count (get-check-count g)
      :timeout.debug/sample-count (get-sample-count g)
      :timeout.debug/panic-count (get-panic-count g)

      :timeout.debug/stride-final (get-stride-k g)
      :timeout.debug/stride-min (get-stride-min g)
      :timeout.debug/stride-max (get-stride-max g)
      :timeout.debug/stride-shrink-count (get-stride-shrink-count g)

      :timeout.debug/mean-step-us mu
      :timeout.debug/var-step-us v2
      :timeout.debug/p95-proxy-us p95
      :timeout.debug/decayed-max-us (get-decayed-max-us g)
      :timeout.debug/effective-step-us (max p95 (get-decayed-max-us g) 1.0)
      :timeout.debug/burn-max (get-burn-max g)}}))

;; ---------- Public API ----------

(defn make-timeout
  "Create a `timeout?` function bound to the given deadline (milliseconds).
   Usage:
     (let [timeout? (make-timeout 200)]
       (reduce (fn [acc x]
                 (if (timeout?) (reduced acc) (step acc x)))
               init xs))
   Debug:
     (timeout? :timeout/debug) => {:timeout/timeout? <bool>, :timeout/debug {...}}"
  [timeout-ms]
  (let [start (now-us)
        deadline #?(:clj (+ start (long (unchecked-multiply 1000 timeout-ms)))
                    :cljs (+ start (js/Math.floor (* 1000 timeout-ms))))
        target-ov #?(:clj (long (unchecked-multiply
                                 1000 (min TARGET-OVERSHOOT-CAP-MS
                                           (* TARGET-OVERSHOOT-FRACTION timeout-ms))))
                     :cljs (js/Math.floor (* 1000 (min TARGET-OVERSHOOT-CAP-MS
                                                       (* TARGET-OVERSHOOT-FRACTION timeout-ms)))))
        stride-min-init #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER)
        g (Guard. deadline target-ov
                  start
                  start 0 1
                  10.0 10.0 10.0
                  false 0
                  0 0 0
                  stride-min-init 1 0 0.0)]
    (fn
      ([] (timeout?-impl g)) ; hot path: boolean (true => stop now)
      ([op]
       (case op
         :timeout/debug (debug-map g)
         (throw (ex-info "Unknown op for timeout? function" {:op op})))))))