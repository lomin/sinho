(ns me.lomin.sinho.timeout-platform-test
  "Platform-specific tests for timeout implementation"
  (:require [clojure.test :refer [deftest is testing]]
            [me.lomin.sinho.timeout :as timeout]))

;; ----- Platform-specific Time Source Tests -----

#?(:cljs
   (deftest cljs-time-source-fallback-test
     (testing "CLJS time source hierarchy validation"
       ;; Test the fallback chain: hrtime BigInt -> performance.now -> Date.now
       ;; We can't easily mock the environment, but we can test what's available

       (testing "Available time sources"
         (let [has-process (exists? js/process)
               has-hrtime (and has-process (.-hrtime js/process))
               has-hrtime-bigint (and has-hrtime (.-bigint (.-hrtime js/process)))
               has-performance (exists? js/performance)
               has-date (exists? js/Date)]

           ;; At least Date should always be available
           (is has-date "Date should always be available in JS")

           ;; Log available time sources for debugging
           (println (str "Time sources available:"))
           (println (str "  process.hrtime.bigint: " has-hrtime-bigint))
           (println (str "  performance.now: " has-performance))
           (println (str "  Date.now: " has-date))))

       (testing "now-us function returns valid microsecond timestamps"
         (let [t1 (timeout/now-us)
               _ (dotimes [_ 1000] (+ 1 1)) ; Small busy loop delay
               t2 (timeout/now-us)]

           (is (number? t1) "Should return a number")
           (is (number? t2) "Should return a number")
           (is (>= t2 t1) "Should be monotonically increasing")
           (is (pos? t1) "Should be positive")

              ;; For relative time measurements, just ensure it's reasonable
              ;; (Could be process-relative or epoch-relative depending on implementation)
           (is (> t1 0) "Should be a positive number")))

       (testing "Time precision and consistency"
            ;; Test that now-us has adequate precision for timeout measurements
         (let [measurements (repeatedly 20 #(timeout/now-us))
               deltas (map - (rest measurements) measurements)
               positive-deltas (filter pos? deltas)]

              ;; Verify monotonicity
           (is (every? #(>= % 0) deltas) "Time should be monotonic (non-decreasing)")

              ;; Should have some measurable differences over multiple calls
           (is (>= (count positive-deltas) 1) "Should have some measurable time differences")

              ;; Verify precision is in microseconds (differences should be small integers)
           (when (seq positive-deltas)
             (let [min-delta (apply min positive-deltas)]
               (is (and (number? min-delta) (> min-delta 0))
                   "Should have microsecond-level precision"))))))))

#?(:clj
   (deftest clj-time-source-test
     (testing "JVM time source validation"
       (testing "System.nanoTime provides adequate precision"
         (let [t1-ns (System/nanoTime)
               t1-us (quot t1-ns 1000)
               _ (Thread/sleep 1)
               t2-ns (System/nanoTime)
               t2-us (quot t2-ns 1000)]

           (is (> t2-us t1-us) "Should be monotonically increasing")
           (is (pos? t1-us) "Should be positive")
           (is (> (- t2-us t1-us) 500) "Should capture millisecond delays")))

       (testing "now-us function matches System.nanoTime precision"
         (let [sys-us (quot (System/nanoTime) 1000)
               timeout-us (timeout/now-us)
               diff (Math/abs (- timeout-us sys-us))]

           ;; Should be very close (within microseconds)
           (is (< diff 1000) "Should match System.nanoTime closely")
           (is (number? timeout-us) "Should return a number"))))))

;; ----- Cross-platform Consistency Tests -----

(deftest cross-platform-behavior-test
  (testing "Timeout behavior consistency across platforms"
    (let [timeout-ms 100
          timeout? (timeout/make-timeout timeout-ms)]

      ;; Initial state should be consistent
      (is (false? (timeout?)) "Should start as not timed out")

      ;; Debug payload structure should be consistent
      (let [debug (:timeout/debug (timeout? :timeout/debug))]
        (is (contains? debug :timeout.debug/start-us) "Should have start timestamp")
        (is (contains? debug :timeout.debug/deadline-us) "Should have deadline")
        (is (contains? debug :timeout.debug/target-overshoot-us) "Should have target overshoot")
        (is (contains? debug :timeout.debug/check-count) "Should have check count")
        (is (contains? debug :timeout.debug/sample-count) "Should have sample count")
        (is (contains? debug :timeout.debug/mean-step-us) "Should have mean step")
        (is (contains? debug :timeout.debug/var-step-us) "Should have variance")

        ;; Values should be reasonable regardless of platform
        (let [start (:timeout.debug/start-us debug)
              deadline (:timeout.debug/deadline-us debug)
              target-overshoot (:timeout.debug/target-overshoot-us debug)]

          (is (pos? start) "Start should be positive")
          (is (> deadline start) "Deadline should be after start")
          (is (= (- deadline start) (* 1000 timeout-ms)) "Deadline calculation should be consistent")
          (is (= target-overshoot 5000) "Target overshoot should be min(10ms, 5% of 100ms) = 5ms")))))

  (testing "Algorithm constants are platform-independent"
    ;; These should be identical across all platforms
    (is (= timeout/Z95 1.64485) "Z95 should be consistent")
    (is (= timeout/ALPHA 0.05) "ALPHA should be consistent")
    (is (= timeout/SAFETY 2.0) "SAFETY should be consistent")
    (is (= timeout/DECAY 0.98) "DECAY should be consistent")
    (is (= timeout/BURN-RATIO 0.33) "BURN-RATIO should be consistent")
    (is (= timeout/KMAX (bit-shift-left 1 20)) "KMAX should be consistent"))

  (testing "Timeout detection timing across platforms"
    (let [timeout-ms 50 ; Short timeout for quick test
          timeout? (timeout/make-timeout timeout-ms)
          start-time #?(:clj (System/currentTimeMillis) :cljs (.now js/Date))]

      ;; Wait past the timeout
      #?(:clj (Thread/sleep (+ timeout-ms 10))
         :cljs (let [wait-start (.now js/Date)]
                 (while (< (- (.now js/Date) wait-start) (+ timeout-ms 10)) nil)))

      (is (true? (timeout?)) "Should detect timeout")

      (let [end-time #?(:clj (System/currentTimeMillis) :cljs (.now js/Date))
            elapsed-ms (- end-time start-time)]

        ;; Should have waited approximately the right amount of time
        (is (>= elapsed-ms timeout-ms) "Should have waited at least the timeout period")
        (is (< elapsed-ms (* 2 timeout-ms)) "Should not have waited excessively long")))))

;; ----- Platform-specific Performance Tests -----

(deftest platform-performance-characteristics-test
  (testing "Fast path performance is adequate"
    (let [timeout? (timeout/make-timeout 10000) ; Long timeout to stay on fast path
          iterations 1000
          start-time #?(:clj (System/nanoTime) :cljs (.now js/Date))]

      ;; Make many fast-path calls
      (dotimes [_ iterations]
        (timeout?))

      (let [end-time #?(:clj (System/nanoTime) :cljs (.now js/Date))
            total-time-us #?(:clj (quot (- end-time start-time) 1000)
                             :cljs (* 1000 (- end-time start-time)))
            avg-time-per-call-us (/ total-time-us iterations)]

        ;; Fast path should be very fast (sub-microsecond on modern hardware)
        (is (< avg-time-per-call-us 10) ; Less than 10µs per call
            (str "Fast path too slow: " avg-time-per-call-us "µs per call"))

        ;; Verify we stayed on fast path
        (let [debug (:timeout/debug (timeout? :timeout/debug))
              sample-count (:timeout.debug/sample-count debug)
              check-count (:timeout.debug/check-count debug)
              sampling-ratio (/ (double sample-count) (double check-count))]

          ;; Should have very low sampling ratio for fast path
          (is (< sampling-ratio 0.1) ; Less than 10% sampling
              (str "Too much sampling on fast path: " sampling-ratio))))))

  (testing "Sample path overhead is reasonable"
    (let [timeout? (timeout/make-timeout 1000) ; Longer timeout
          sample-calls 10]

      ;; Force sampling by making calls with delays
      (dotimes [_ sample-calls]
        #?(:clj (Thread/sleep 1) :cljs nil)
        (timeout?))

      (let [debug (:timeout/debug (timeout? :timeout/debug))
            sample-count (:timeout.debug/sample-count debug)]

        ;; Should have forced some sampling
        (is (> sample-count 0) "Should have triggered sampling")
        (is (<= sample-count sample-calls) "Should not sample more than call count")))))

#?(:clj
   (deftest jvm-unchecked-math-test
     (testing "Unchecked math optimization on JVM"
       ;; This is more of a smoke test - we can't easily verify the actual
       ;; unchecked operations, but we can ensure they don't cause overflow issues
       (let [timeout? (timeout/make-timeout 1000)]

         ;; Make many calls to exercise unchecked math paths
         (dotimes [_ 10000] (timeout?))

         (let [debug (:timeout/debug (timeout? :timeout/debug))
               check-count (:timeout.debug/check-count debug)]

           ;; Should handle large counter values without overflow
           (is (>= check-count 10000) "Should handle large check counts")
           (is (pos? check-count) "Check count should remain positive"))))))

;; ----- Memory and GC Impact Tests -----

(deftest memory-allocation-test
  (testing "Timeout creation doesn't cause excessive allocation"
    ;; Create many timeout instances to test allocation patterns
    (let [timeout-count 100
          timeouts (repeatedly timeout-count #(timeout/make-timeout 1000))]

      ;; All should be functional
      (is (= (count timeouts) timeout-count) "Should create all timeouts")
      (is (every? fn? timeouts) "All should be functions")

      ;; Basic functionality check
      (doseq [timeout-fn timeouts]
        (is (false? (timeout-fn)) "Should start as not timed out")
        (is (map? (timeout-fn :timeout/debug)) "Should provide debug info"))))

  (testing "Repeated calls don't cause memory leaks"
    (let [timeout? (timeout/make-timeout 5000) ; Long timeout
          call-count 1000]

      ;; Make many repeated calls
      (dotimes [_ call-count] (timeout?))

      (let [debug (:timeout/debug (timeout? :timeout/debug))]
        ;; State should remain reasonable
        (is (number? (:timeout.debug/mean-step-us debug)) "Mean should be numeric")
        (is (< (:timeout.debug/var-step-us debug) 1e10) "Variance should be bounded")
        (is (pos? (:timeout.debug/check-count debug)) "Check count should be positive")))))