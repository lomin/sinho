(ns me.lomin.sinho.timeout-optimized
  "Optimized timeout implementation with minimal atom operations")

;; Copy time and constants from original
#?(:clj
   (defn now-us [] (quot (System/nanoTime) 1000))
   :cljs
   (defn now-us []
     (if (and (exists? js/process) (.-hrtime js/process) (.-bigint (.-hrtime js/process)))
       (let [bn ((.-bigint (.-hrtime js/process)))]
         (js/Number (/ bn (js/BigInt 1000))))
       (int (* 1000 (if (exists? js/performance) (js/performance.now) (.now js/Date)))))))

(def ^:const Z95 1.64485)
(def ^:const ALPHA 0.05)
(def ^:const SAFETY 2.0)
(def ^:const DECAY 0.98)
(def ^:const BURN-RATIO 0.33)
(def ^:const KMAX (bit-shift-left 1 20))

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

;; OPTIMIZED: Single swap operation
(defn- timeout?-impl [guard-atom]
  (let [[should-timeout? _]
        (swap-vals! guard-atom
                    (fn [state]
                      (let [s #?(:clj (unchecked-inc (:steps-since state))
                                 :cljs (inc (:steps-since state)))]
                        ;; Fast path - just increment counters
                        (if (< s (:stride-k state))
                          (-> state
                              (assoc :steps-since s)
                              (update :check-count #?(:clj unchecked-inc :cljs inc)))

                          ;; Sample path - full update in single operation  
                          (let [now (now-us)
                                remaining #?(:clj (unchecked-subtract (:deadline-us state) now)
                                             :cljs (- (:deadline-us state) now))]
                            (if (<= remaining 0)
                              ;; Timeout case
                              (-> state
                                  (assoc :steps-since s :timed-out true)
                                  (update :check-count #?(:clj unchecked-inc :cljs inc))
                                  (update :sample-count #?(:clj unchecked-inc :cljs inc))
                                  (update :detected-at-us #(if (zero? %) now %)))

                              ;; Normal sampling case - all updates in one go
                              (let [elapsed #?(:clj (unchecked-subtract now (:last-sample-us state))
                                               :cljs (- now (:last-sample-us state)))
                                    x (/ (double elapsed) (double s))
                                    a ALPHA
                                    mu (:mean-us state)
                                    d (- x mu)
                                    mu' (+ mu (* a d))
                                    var' (min (+ (* (- 1.0 a) (:var-us state)) (* a d d)) 1.0e18)
                                    decayed-max' (max x (* DECAY (:decayed-max-us state)))

                                    burn (if (pos? remaining) (/ (double elapsed) (double remaining)) 1.0)
                                    panic? (> burn BURN-RATIO)

                                    temp-state (assoc state :mean-us mu' :var-us var' :decayed-max-us decayed-max')
                                    new-k (if panic? 1 (calc-stride temp-state remaining))
                                    old-k (:stride-k state)]

                                ;; Single comprehensive update
                                (-> state
                                    (assoc :steps-since 0
                                           :stride-k new-k
                                           :mean-us mu'
                                           :var-us var'
                                           :decayed-max-us decayed-max'
                                           :last-sample-us now)
                                    (update :check-count #?(:clj unchecked-inc :cljs inc))
                                    (update :sample-count #?(:clj unchecked-inc :cljs inc))
                                    (update :burn-max #(max % burn))
                                    (cond-> panic? (update :panic-count #?(:clj unchecked-inc :cljs inc)))
                                    (cond-> (< new-k old-k) (update :stride-shrink-count #?(:clj unchecked-inc :cljs inc)))
                                    (cond-> (> new-k (:stride-max state)) (assoc :stride-max new-k))
                                    (cond-> (< new-k (:stride-min state)) (assoc :stride-min new-k))))))))))]

    ;; Return timeout status
    (or (:timed-out should-timeout?)
        (and (>= (:steps-since should-timeout?) (:stride-k should-timeout?))
             (:timed-out should-timeout?)))))

;; Copy debug-map and make-timeout from original
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

(defn make-timeout [timeout-ms]
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