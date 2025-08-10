(ns me.lomin.sinho.timeout
  "Adaptive Self-Clocking Timeout Guard for CLJ/CLJS")

;; ----- Time (µs) -----

#?(:clj
   (defn now-us [] (quot (System/nanoTime) 1000))
   :cljs
   (defn now-us []
     (if (and (exists? js/process) (.-hrtime js/process) (.-bigint (.-hrtime js/process)))
       (let [bn ((.-bigint (.-hrtime js/process)))]
         (js/Number (/ bn (js/BigInt 1000))))
       (int (* 1000 (if (exists? js/performance) (js/performance.now) (.now js/Date)))))))

;; ----- Constants -----

(def ^:const Z95 1.64485)
(def ^:const ALPHA 0.05)
(def ^:const SAFETY 2.0)
(def ^:const DECAY 0.98)
(def ^:const BURN-RATIO 0.33)
(def ^:const KMAX (bit-shift-left 1 20))

;; ----- Guard using atom for mutable state -----

(defn- make-guard-state [deadline target-overshoot start]
  {:deadline-us deadline
   :target-overshoot-us target-overshoot
   :original-start-us start
   :last-sample-us start
   :steps-since 0
   :stride-k 1
   :mean-us 10.0
   :var-us 10.0
   :decayed-max-us 10.0
   :timed-out false
   :detected-at-us 0
   :check-count 0
   :sample-count 0
   :panic-count 0
   :stride-min #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER)
   :stride-max 1
   :stride-shrink-count 0
   :burn-max 0.0})

(defn- calc-effective [state]
  (let [mu (:mean-us state)
        v (max (:var-us state) 0.0)
        p95 (+ mu (* Z95 (Math/sqrt v)))]
    (max p95 (:decayed-max-us state) 1.0)))

(defn- calc-stride [state remaining-us]
  (let [eff (calc-effective state)
        budget (max (- remaining-us (:target-overshoot-us state))
                    (:target-overshoot-us state))
        k (#?(:clj long :cljs int) (Math/floor (/ (double budget) (* SAFETY eff))))]
    (-> k (max 1) (min KMAX))))

(defn- timeout?-impl [guard-atom]
  (let [state (swap! guard-atom update :steps-since #?(:clj unchecked-inc :cljs inc))
        _ (swap! guard-atom update :check-count #?(:clj unchecked-inc :cljs inc))
        s (:steps-since state)]

    (if (< s (:stride-k state))
      false

      (let [now (now-us)
            remaining #?(:clj (unchecked-subtract (:deadline-us state) now)
                         :cljs (- (:deadline-us state) now))]
        (swap! guard-atom update :sample-count #?(:clj unchecked-inc :cljs inc))

        (if (<= remaining 0)
          (do
            (swap! guard-atom assoc :timed-out true)
            (swap! guard-atom update :detected-at-us #(if (zero? %) now %))
            true)

          (let [elapsed #?(:clj (unchecked-subtract now (:last-sample-us state))
                           :cljs (- now (:last-sample-us state)))
                x (/ (double elapsed) (double s))
                a ALPHA
                mu (:mean-us state)
                d (- x mu)
                mu' (+ mu (* a d))]

            ;; Update statistics
            (swap! guard-atom assoc :mean-us mu')
            (swap! guard-atom update :var-us #(min (+ (* (- 1.0 a) %) (* a d d)) 1.0e18))
            (swap! guard-atom update :decayed-max-us #(max x (* DECAY %)))

            (let [state @guard-atom
                  burn (if (pos? remaining) (/ (double elapsed) (double remaining)) 1.0)
                  panic? (> burn BURN-RATIO)
                  new-k (if panic? 1 (calc-stride state remaining))
                  old-k (:stride-k state)]

              ;; Update debug counters and reset
              (swap! guard-atom (fn [s]
                                  (cond-> s
                                    (> burn (:burn-max s)) (assoc :burn-max burn)
                                    panic? (update :panic-count #?(:clj unchecked-inc :cljs inc))
                                    (< new-k old-k) (update :stride-shrink-count #?(:clj unchecked-inc :cljs inc))
                                    (> new-k (:stride-max s)) (assoc :stride-max new-k)
                                    (< new-k (:stride-min s)) (assoc :stride-min new-k)
                                    true (assoc :last-sample-us now
                                                :steps-since 0
                                                :stride-k new-k))))
              false)))))))

(defn- debug-map [guard-atom]
  (let [state @guard-atom
        now (now-us)
        deadline (:deadline-us state)
        overshoot (max 0 #?(:clj (unchecked-subtract now deadline)
                            :cljs (- now deadline)))
        mu (:mean-us state)
        v2 (max (:var-us state) 0.0)
        p95 (+ mu (* Z95 (Math/sqrt v2)))
        eff (max p95 (:decayed-max-us state) 1.0)]
    {:timeout/timeout? (:timed-out state)
     :timeout/debug
     {:timeout.debug/start-us (:original-start-us state)
      :timeout.debug/now-us now
      :timeout.debug/deadline-us deadline
      :timeout.debug/detected-at-us (:detected-at-us state)
      :timeout.debug/overshoot-us overshoot
      :timeout.debug/target-overshoot-us (:target-overshoot-us state)

      :timeout.debug/check-count (:check-count state)
      :timeout.debug/sample-count (:sample-count state)
      :timeout.debug/panic-count (:panic-count state)

      :timeout.debug/stride-final (:stride-k state)
      :timeout.debug/stride-min (:stride-min state)
      :timeout.debug/stride-max (:stride-max state)
      :timeout.debug/stride-shrink-count (:stride-shrink-count state)

      :timeout.debug/mean-step-us mu
      :timeout.debug/var-step-us v2
      :timeout.debug/p95-proxy-us p95
      :timeout.debug/decayed-max-us (:decayed-max-us state)
      :timeout.debug/effective-step-us eff
      :timeout.debug/burn-max (:burn-max state)}}))

(defn make-timeout
  "Create a `timeout?` function bound to the given deadline (milliseconds).
   Usage:
     (let [timeout? (make-timeout 100)]
       (when (timeout?) ...stop...))
   Debug:
     (timeout? :timeout/debug) => map with :timeout/timeout? and :timeout/debug"
  [timeout-ms]
  (let [start (now-us)
        deadline (+ start (#?(:clj long :cljs int) (* 1000 timeout-ms)))
        target-ov (#?(:clj long :cljs int) (* 1000 (min 10.0 (* 0.05 timeout-ms))))
        guard-atom (atom (make-guard-state deadline target-ov start))]
    (fn
      ([] (timeout?-impl guard-atom))
      ([op]
       (case op
         :timeout/debug (debug-map guard-atom)
         (throw (ex-info "Unknown op for timeout? function" {:op op})))))))