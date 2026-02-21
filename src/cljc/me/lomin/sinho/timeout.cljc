(ns me.lomin.sinho.timeout
  "Adaptive Self-Clocking Timeout Guard for CLJ/CLJS

  This implementation provides a cooperative timeout mechanism that can efficiently
  stop CPU-bound computations after a specified timeout period without requiring
  external watchdog threads or complex coordination.

  ## Overview

  The timeout guard uses an A* search-inspired adaptive algorithm that automatically
  adjusts how frequently it checks the system clock based on observed timing patterns.
  This minimizes the overhead when far from the timeout deadline while ensuring
  responsive detection near the timeout boundary.

  ## Key Algorithm Components

  ### Adaptive Stride-Based Sampling
  Instead of checking the clock on every call, the algorithm dynamically calculates
  a 'stride' - the number of calls to skip before the next time check. This stride
  is continuously adapted based on:
  
  - **EWMA (Exponentially Weighted Moving Average)**: Tracks mean time per step
  - **EWVAR (Exponentially Weighted Variance)**: Estimates timing variability  
  - **Decayed Maximum**: Hedges against occasional long steps that could cause overshoot
  - **Panic Mode**: Forces frequent sampling when time is being consumed too rapidly

  ### Target Overshoot Envelope
  The algorithm aims to detect timeouts within a bounded overshoot envelope:
  ```
  p99 overshoot ≤ max(S, min(5% × timeout, 10ms))
  ```
  where S is the p99 cost of a single atomic step between checks.

  ### Time-Based Ceiling
  To prevent excessively long periods without sampling, the stride calculation
  includes a time-based ceiling that ensures samples occur at least every 10ms
  of wall-clock time.

  ## Usage Pattern

  ```clojure
  (let [timeout? (make-timeout 200)]          ; 200ms timeout
    (reduce (fn [acc x]
              (if (timeout?)                  ; Check at natural boundaries
                (reduced acc)                 ; Return current value on timeout
                (expensive-step acc x)))      ; Continue processing
            initial-value
            large-dataset))
  ```

  ## Performance Characteristics

  - **Fast Path**: Simple counter increment and comparison (zero allocation)
  - **Sample Path**: Single monotonic time read plus statistical updates
  - **Steady-State Overhead**: Target <1-2% when far from deadline
  - **Cross-Platform**: Works on JVM, Node.js, and modern browsers

  ## Thread Confinement Model
  Each timeout guard is designed for single-threaded use (one guard per compute thread).
  Multiple guards can run concurrently in different threads without coordination.

  ## Constants and Tuning
  The algorithm uses fixed constants tuned for general-purpose workloads:
  - Z95 = 1.64485 (95th percentile multiplier for normal distribution)
  - ALPHA = 0.05 (EWMA/EWVAR smoothing factor)
  - SAFETY = 2.0 (conservative stride multiplier)
  - DECAY = 0.98 (decayed maximum decay rate per sample)
  - BURN_RATIO = 0.33 (panic threshold: >33% of remaining time consumed)

  These constants are intentionally not configurable to maintain API simplicity
  and ensure consistent behavior across deployments.")

#?(:clj (set! *warn-on-reflection* true))

;; ---------- Time (µs) ----------

;; Cross-platform monotonic time source in microseconds
;; 
;; JVM: Uses System/nanoTime() which provides high-resolution, monotonic time
;; that is not affected by system clock adjustments. Converted to microseconds
;; for consistent precision across platforms.
;;
;; Node.js: Uses process.hrtime.bigint() when available, which provides
;; nanosecond precision monotonic time. Falls back to performance.now() or
;; Date.now() in other JS environments.
;;
;; Browser: Uses performance.now() for sub-millisecond precision when available,
;; otherwise falls back to Date.now(). Performance.now() is relative to
;; navigationStart and is monotonic within the browser session.
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

;; Algorithm constants - carefully tuned for general-purpose workloads
(def ^:const ^double Z95 1.64485) ; 95th percentile multiplier for normal distribution
(def ^:const ^double ALPHA 0.05) ; EWMA/EWVAR smoothing factor
(def ^:const ^double SAFETY 2.0) ; Conservative stride multiplier
(def ^:const ^double DECAY 0.98) ; Decayed maximum decay rate per sample
(def ^:const ^double BURN-RATIO 0.33) ; Panic threshold: >33% of remaining time consumed
(def ^:const ^long KMAX (bit-shift-left 1 20)) ; Maximum stride value (1M steps)
(def ^:const ^double SAMPLE-MAX-SPACING-MS 10.0) ; Time-based ceiling: max 10ms between samples
(def ^:const ^double TARGET-OVERSHOOT-FRACTION 0.05) ; Target overshoot: 5% of timeout
(def ^:const ^double TARGET-OVERSHOOT-CAP-MS 10.0) ; Maximum target overshoot: 10ms

;; ---------- Guard Protocol ----------

;; Guard State Management Protocol
;;
;; The Guard uses a protocol-based approach for field access to provide
;; better encapsulation while maintaining performance. All mutable fields
;; are accessed through getter/setter methods rather than direct field access.
;;
;; The Guard tracks both immutable configuration (deadline, target overshoot)
;; and mutable runtime state (timing statistics, stride calculations, debug counters).

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

#?(:bb
   (defrecord BBGuard [deadline-us target-overshoot-us start-us state]
     IGuard
     ;; Immutable field getters
     (get-deadline-us [_] deadline-us)
     (get-target-overshoot-us [_] target-overshoot-us)
     (get-start-us [_] start-us)

     ;; Mutable field getters — deref volatile state map
     (get-last-sample-us [_] (:last-sample-us @state))
     (get-steps-since [_] (:steps-since @state))
     (get-stride-k [_] (:stride-k @state))
     (get-mean-us [_] (:mean-us @state))
     (get-var-us [_] (:var-us @state))
     (get-decayed-max-us [_] (:decayed-max-us @state))
     (get-timed-out [_] (:timed-out @state))
     (get-detected-at-us [_] (:detected-at-us @state))
     (get-check-count [_] (:check-count @state))
     (get-sample-count [_] (:sample-count @state))
     (get-panic-count [_] (:panic-count @state))
     (get-stride-min [_] (:stride-min @state))
     (get-stride-max [_] (:stride-max @state))
     (get-stride-shrink-count [_] (:stride-shrink-count @state))
     (get-burn-max [_] (:burn-max @state))

     ;; Mutable field setters — vswap! on volatile state map
     ;; Each setter returns the coerced value (matching deftype set! semantics)
     (set-last-sample-us! [_ val] (let [v (long val)] (vswap! state assoc :last-sample-us v) v))
     (set-steps-since! [_ val] (let [v (long val)] (vswap! state assoc :steps-since v) v))
     (set-stride-k! [_ val] (let [v (long val)] (vswap! state assoc :stride-k v) v))
     (set-mean-us! [_ val] (let [v (double val)] (vswap! state assoc :mean-us v) v))
     (set-var-us! [_ val] (let [v (double val)] (vswap! state assoc :var-us v) v))
     (set-decayed-max-us! [_ val] (let [v (double val)] (vswap! state assoc :decayed-max-us v) v))
     (set-timed-out! [_ val] (let [v (boolean val)] (vswap! state assoc :timed-out v) v))
     (set-detected-at-us! [_ val] (let [v (long val)] (vswap! state assoc :detected-at-us v) v))
     (set-check-count! [_ val] (let [v (long val)] (vswap! state assoc :check-count v) v))
     (set-sample-count! [_ val] (let [v (long val)] (vswap! state assoc :sample-count v) v))
     (set-panic-count! [_ val] (let [v (long val)] (vswap! state assoc :panic-count v) v))
     (set-stride-min! [_ val] (let [v (long val)] (vswap! state assoc :stride-min v) v))
     (set-stride-max! [_ val] (let [v (long val)] (vswap! state assoc :stride-max v) v))
     (set-stride-shrink-count! [_ val] (let [v (long val)] (vswap! state assoc :stride-shrink-count v) v))
     (set-burn-max! [_ val] (let [v (double val)] (vswap! state assoc :burn-max v) v)))

   :default
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
     (set-burn-max! [_ val] (set! burn-max (double val)))))

;; Core Algorithm Functions
;;
;; These functions implement the heart of the adaptive timeout algorithm:

(defn- calc-effective
  "Calculate effective step time for stride computation.
   
   Combines three estimates to get a conservative p95 step time:
   - EWMA-based p95 proxy using normal distribution assumption
   - Decayed maximum as a hedge against outlier steps  
   - Minimum floor of 1.0µs to prevent division by zero
   
   Returns the maximum of these three values."
  [^Guard g]
  (let [mu (get-mean-us g)
        v (max (get-var-us g) 0.0)
        p95 (+ mu (* Z95 (Math/sqrt v)))]
    (max p95 (get-decayed-max-us g) 1.0)))

(defn- calc-stride
  "Calculate adaptive stride for next sampling window.
   
   Determines how many timeout? calls to make before next time check.
   Uses two constraints:
   
   1. Budget-based: Allocates remaining time budget considering target overshoot
   2. Time-based ceiling: Ensures sampling at least every 10ms wall-clock time
   
   The stride is the minimum of these constraints, clamped between 1 and KMAX.
   Uses SAFETY multiplier for conservative estimation."
  [^Guard g remaining-us]
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

;; Main timeout check implementation with two distinct paths:
;;
;; Fast Path: Simple counter increment and comparison (zero allocation)
;; - Increments step counter and check counter
;; - Returns false if stride not yet reached
;;
;; Sample Path: Time check and statistical updates
;; - Reads monotonic clock and checks deadline
;; - Updates EWMA/EWVAR statistics and decayed maximum  
;; - Calculates new stride with panic mode detection
;; - Resets sampling window for next iteration

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
  "Create an adaptive timeout guard function for cooperative CPU-bound computation limits.
  
   Creates a `timeout?` function that uses adaptive stride-based sampling to efficiently
   check if the specified timeout has been exceeded. The algorithm automatically adjusts
   its sampling frequency based on observed timing patterns to minimize overhead while
   ensuring responsive timeout detection.
   
   Arguments:
   - timeout-ms: Timeout duration in milliseconds (converted to microseconds internally)
   
   Returns:
   A timeout? function with two arities:
   - (timeout?)              => boolean  ; true means STOP NOW, false means continue
   - (timeout? :timeout/debug) => map     ; debugging information and statistics
   
   Usage Pattern:
   ```clojure
   (let [timeout? (make-timeout 200)]  ; 200ms timeout
     (reduce (fn [acc x]
               (if (timeout?)         ; Check at natural loop boundaries
                 (reduced acc)        ; Return current result on timeout  
                 (expensive-step acc x))) ; Continue processing
             initial-value
             large-dataset))
   ```
   
   Performance Notes:
   - Fast path: ~1-2ns overhead (counter increment + comparison)
   - Sample path: ~100ns overhead (time read + statistics)
   - Zero allocation on fast path
   - Adaptive sampling reduces checks when far from deadline
   - Target p99 overshoot: max(step_time, min(5% × timeout, 10ms))
   
   Thread Safety:
   Each timeout guard is designed for single-threaded use. Create one guard per
   compute thread - guards do not require synchronization between threads."
  [timeout-ms]
  (let [start (now-us)
        deadline #?(:clj (+ start (long (unchecked-multiply 1000 timeout-ms)))
                    :cljs (+ start (js/Math.floor (* 1000 timeout-ms))))
        target-ov #?(:clj (long (unchecked-multiply
                                 1000 (min TARGET-OVERSHOOT-CAP-MS
                                           (* TARGET-OVERSHOOT-FRACTION timeout-ms))))
                     :cljs (js/Math.floor (* 1000 (min TARGET-OVERSHOOT-CAP-MS
                                                       (* TARGET-OVERSHOOT-FRACTION timeout-ms)))))
        ;; Initial values for mutable fields
        initial-last-sample-us start ; Start tracking from current time
        initial-steps-since 0 ; No steps taken yet
        initial-stride-k 1 ; Start with stride=1 to force first sample
        initial-mean-us 10.0 ; Conservative initial estimate: 10µs per step
        initial-var-us 10.0 ; Initial variance matches mean (high uncertainty)
        initial-decayed-max-us 10.0 ; Initial max matches mean estimate
        initial-timed-out false ; Not timed out initially
        initial-detected-at-us 0 ; 0 means not yet detected
        initial-check-count 0 ; No checks performed yet
        initial-sample-count 0 ; No samples taken yet
        initial-panic-count 0 ; No panic events yet
        initial-stride-min #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER) ; Track minimum stride seen
        initial-stride-max 1 ; Track maximum stride seen
        initial-stride-shrink-count 0 ; Count stride decreases
        initial-burn-max 0.0 ; Track maximum burn ratio

        g #?(:bb (->BBGuard deadline target-ov start
                            (volatile! {:last-sample-us initial-last-sample-us
                                        :steps-since initial-steps-since
                                        :stride-k initial-stride-k
                                        :mean-us initial-mean-us
                                        :var-us initial-var-us
                                        :decayed-max-us initial-decayed-max-us
                                        :timed-out initial-timed-out
                                        :detected-at-us initial-detected-at-us
                                        :check-count initial-check-count
                                        :sample-count initial-sample-count
                                        :panic-count initial-panic-count
                                        :stride-min initial-stride-min
                                        :stride-max initial-stride-max
                                        :stride-shrink-count initial-stride-shrink-count
                                        :burn-max initial-burn-max}))
             :default (Guard. deadline target-ov
                              start
                              initial-last-sample-us initial-steps-since initial-stride-k
                              initial-mean-us initial-var-us initial-decayed-max-us
                              initial-timed-out initial-detected-at-us
                              initial-check-count initial-sample-count initial-panic-count
                              initial-stride-min initial-stride-max initial-stride-shrink-count
                              initial-burn-max))]
    (fn
      ([] (timeout?-impl g))
      ([op]
       (case op
         :timeout/debug (debug-map g)
         (throw (ex-info "Unknown op for timeout? function" {:op op})))))))