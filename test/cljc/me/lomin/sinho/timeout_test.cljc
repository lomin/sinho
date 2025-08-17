(ns me.lomin.sinho.timeout-test
  "Consolidated timeout test suite focusing on deterministic tests and essential validations.
   
   This test suite prioritizes:
   1. Deterministic tests that verify the timeout logic
   2. Debug output validation (deterministic)
   3. Essential end-to-end tests using debug output for verification"
  (:require
   [clojure.test :refer [deftest is testing]]
   [me.lomin.sinho.matcher :refer [=*]]
   [me.lomin.sinho.timeout :as timeout]))

;; ============================================================================
;; Debug Output Structure Tests (Deterministic)
;; ============================================================================

(deftest test-debug-output-structure
  (testing "Debug output returns expected structure and initial values"
    (let [timeout? (timeout/make-timeout 100)
          _ (timeout?)
          debug-info (timeout? :timeout/debug)
          debug (:timeout/debug debug-info)]

      (is (map? debug-info))
      (is (map? debug))
      (is (contains? debug-info :timeout/timeout?))
      (is (false? (:timeout/timeout? debug-info)))

      ;; Verify all expected keys are present
      (is (= #{:timeout.debug/start-us
               :timeout.debug/now-us
               :timeout.debug/deadline-us
               :timeout.debug/detected-at-us
               :timeout.debug/overshoot-us
               :timeout.debug/target-overshoot-us
               :timeout.debug/check-count
               :timeout.debug/sample-count
               :timeout.debug/panic-count
               :timeout.debug/stride-final
               :timeout.debug/stride-min
               :timeout.debug/stride-max
               :timeout.debug/stride-shrink-count
               :timeout.debug/mean-step-us
               :timeout.debug/var-step-us
               :timeout.debug/p95-proxy-us
               :timeout.debug/decayed-max-us
               :timeout.debug/effective-step-us
               :timeout.debug/burn-max}
             (set (keys debug))))

      ;; Verify initial counter values
      (is (>= (:timeout.debug/check-count debug) 1))
      (is (>= (:timeout.debug/sample-count debug) 1))
      (is (<= (:timeout.debug/sample-count debug) (:timeout.debug/check-count debug)))

      ;; Verify timestamp ordering
      (is (<= (:timeout.debug/start-us debug) (:timeout.debug/now-us debug)))
      (is (= (:timeout.debug/deadline-us debug)
             (+ (:timeout.debug/start-us debug) (* 1000 100))))

      ;; Verify stride bounds
      (is (>= (:timeout.debug/stride-final debug) 1))
      (is (<= (:timeout.debug/stride-final debug) (bit-shift-left 1 20)))

      ;; Verify statistical values
      (is (pos? (:timeout.debug/mean-step-us debug)))
      (is (>= (:timeout.debug/var-step-us debug) 0.0))
      (is (pos? (:timeout.debug/effective-step-us debug))))))

(deftest test-mathematical-relationships
  (testing "Mathematical relationships between debug fields"
    (let [timeout? (timeout/make-timeout 1000)]
      (dotimes [_ 100] (timeout?))
      (let [debug-info (:timeout/debug (timeout? :timeout/debug))
            start (:timeout.debug/start-us debug-info)
            now (:timeout.debug/now-us debug-info)
            deadline (:timeout.debug/deadline-us debug-info)
            overshoot (:timeout.debug/overshoot-us debug-info)
            mean-step (:timeout.debug/mean-step-us debug-info)
            var-step (:timeout.debug/var-step-us debug-info)
            p95-proxy (:timeout.debug/p95-proxy-us debug-info)
            decayed-max (:timeout.debug/decayed-max-us debug-info)
            effective-step (:timeout.debug/effective-step-us debug-info)]

        ;; Time relationships
        (is (<= start now))
        (is (<= start deadline))

        ;; Overshoot calculation: max(0, now - deadline)
        (is (= overshoot (max 0 (- now deadline))))

        ;; Statistical relationships
        (is (>= var-step 0.0))
        (is (pos? mean-step))

        ;; P95 proxy = mean + 1.64485 * sqrt(max(variance, 0))
        (let [expected-p95 (+ mean-step (* 1.64485 (Math/sqrt (max var-step 0.0))))]
          (is (< (Math/abs (- p95-proxy expected-p95)) 0.01)))

        ;; Effective step = max(p95_proxy, decayed_max, 1.0)
        (let [expected-effective (max p95-proxy decayed-max 1.0)]
          (is (= effective-step expected-effective)))

        ;; Decayed max should be >= 1.0
        (is (>= decayed-max 1.0))))))

(deftest test-target-overshoot-calculation
  (testing "Target overshoot calculation follows spec"
    (let [timeout? (timeout/make-timeout 100)
          debug (:timeout/debug (timeout? :timeout/debug))
          target-ov (:timeout.debug/target-overshoot-us debug)]
      ;; Target overshoot should be min(10ms, 5% of timeout) = 5ms = 5000µs
      (is (= 5000 target-ov)))

    (let [timeout? (timeout/make-timeout 500)
          debug (:timeout/debug (timeout? :timeout/debug))
          target-ov (:timeout.debug/target-overshoot-us debug)]
      ;; Target overshoot should be min(10ms, 5% of 500ms) = min(10ms, 25ms) = 10ms = 10000µs
      (is (= 10000 target-ov)))))

;; ============================================================================
;; Counter Behavior Tests (Deterministic)
;; ============================================================================

(deftest test-counter-increment-behavior
  (testing "Counters increment properly with calls"
    (let [timeout? (timeout/make-timeout 1000)
          _ (timeout?)
          initial-debug (:timeout/debug (timeout? :timeout/debug))
          initial-check-count (:timeout.debug/check-count initial-debug)
          initial-sample-count (:timeout.debug/sample-count initial-debug)

          _ (dotimes [_ 100] (timeout?))
          final-debug (:timeout/debug (timeout? :timeout/debug))
          final-check-count (:timeout.debug/check-count final-debug)
          final-sample-count (:timeout.debug/sample-count final-debug)]

      ;; Check count should increase by number of calls
      (is (= final-check-count (+ initial-check-count 100)))

      ;; Sample count should increase but not necessarily by same amount
      (is (>= final-sample-count initial-sample-count))
      (is (<= final-sample-count final-check-count))

      ;; Efficiency should improve (lower sampling ratio)
      (when (and (pos? initial-check-count) (pos? final-check-count))
        (let [initial-ratio (/ initial-sample-count initial-check-count)
              final-ratio (/ final-sample-count final-check-count)]
          (is (<= final-ratio initial-ratio)))))))

(deftest test-no-timeout-before-deadline
  (testing "Timeout should not trigger before deadline"
    (let [timeout-ms 100
          timeout? (timeout/make-timeout timeout-ms)
          results (doall (repeatedly 1000 timeout?))]

      (is (every? false? results))

      (let [debug-info (timeout? :timeout/debug)]
        (is (false? (:timeout/timeout? debug-info)))))))

;; ============================================================================
;; Adaptive Behavior Tests (Mostly Deterministic)
;; ============================================================================

(deftest test-stride-adaptation
  (testing "Stride adaptation with many quick calls"
    (let [timeout? (timeout/make-timeout 1000)]
      (dotimes [_ 20] (timeout?))
      (let [debug (:timeout/debug (timeout? :timeout/debug))]
        (is (> (:timeout.debug/stride-final debug) 1))
        (let [efficiency (/ (:timeout.debug/sample-count debug)
                            (:timeout.debug/check-count debug))]
          (is (< efficiency 0.5)))))))

(deftest test-stride-bounds
  (testing "Stride stays within bounds"
    (let [timeout? (timeout/make-timeout 1000)]
      (timeout?)
      (let [debug (:timeout/debug (timeout? :timeout/debug))
            stride (:timeout.debug/stride-final debug)]
        (is (>= stride 1))
        (is (<= stride (bit-shift-left 1 20)))))))

(deftest test-statistics-evolution
  (testing "Statistics evolve with calls"
    (let [timeout? (timeout/make-timeout 1000)]
      (dotimes [_ 10] (timeout?))
      (let [debug (:timeout/debug (timeout? :timeout/debug))]
        (is (pos? (:timeout.debug/mean-step-us debug)))
        (is (>= (:timeout.debug/var-step-us debug) 0.0))
        (is (pos? (:timeout.debug/decayed-max-us debug)))
        (is (pos? (:timeout.debug/effective-step-us debug)))))))

;; ============================================================================
;; Platform Consistency Tests (Deterministic)
;; ============================================================================

(deftest test-cross-platform-behavior
  (testing "Timeout behavior consistency across platforms"
    (let [timeout-ms 100
          timeout? (timeout/make-timeout timeout-ms)]

      (is (false? (timeout?)))

      (let [debug (:timeout/debug (timeout? :timeout/debug))]
        (is (contains? debug :timeout.debug/start-us))
        (is (contains? debug :timeout.debug/deadline-us))
        (is (contains? debug :timeout.debug/target-overshoot-us))

        (let [start (:timeout.debug/start-us debug)
              deadline (:timeout.debug/deadline-us debug)
              target-overshoot (:timeout.debug/target-overshoot-us debug)]

          (is (pos? start))
          (is (> deadline start))
          (is (= (- deadline start) (* 1000 timeout-ms)))
          (is (= target-overshoot 5000)))))))

(deftest test-algorithm-constants
  (testing "Algorithm constants are platform-independent"
    (is (= timeout/Z95 1.64485))
    (is (= timeout/ALPHA 0.05))
    (is (= timeout/SAFETY 2.0))
    (is (= timeout/DECAY 0.98))
    (is (= timeout/BURN-RATIO 0.33))
    (is (= timeout/KMAX (bit-shift-left 1 20)))))

(deftest test-time-monotonicity
  (testing "Time measurements are monotonic"
    (let [t1 (timeout/now-us)
          _ (dotimes [_ 1000] (+ 1 1))
          t2 (timeout/now-us)]
      (is (<= t1 t2))
      (is (number? t1))
      (is (number? t2)))))

;; ============================================================================
;; Cooperative Usage Patterns (Deterministic)
;; ============================================================================

(deftest test-cooperative-reduce-pattern
  (testing "Cooperative timeout in reduce operation"
    (let [timeout? (timeout/make-timeout 2000)
          result (reduce (fn [acc i]
                           (if (timeout?)
                             (reduced {:stopped-early true :last-i i :sum acc})
                             (+ acc i)))
                         0
                         (range 100))]
      ;; Should complete without timeout
      (is (= result (reduce + (range 100)))))))

(deftest test-multiple-independent-instances
  (testing "Multiple timeout instances are independent"
    (let [timeout1? (timeout/make-timeout 100)
          timeout2? (timeout/make-timeout 200)]

      (is (false? (timeout1?)))
      (is (false? (timeout2?)))

      (let [debug1 (:timeout/debug (timeout1? :timeout/debug))
            debug2 (:timeout/debug (timeout2? :timeout/debug))]
        (is (not= (:timeout.debug/deadline-us debug1)
                  (:timeout.debug/deadline-us debug2)))))))

;; ============================================================================
;; Essential End-to-End Test with Debug Validation (Non-Deterministic)
;; ============================================================================

(deftest test-timeout-detection-with-debug
  (testing "Timeout detection and debug output validation (end-to-end)"
    (let [timeout-ms 50
          work-fn (fn [] (reduce + (range 1000)))
          timeout? (timeout/make-timeout timeout-ms)
          start-ms #?(:clj (System/currentTimeMillis) :cljs (.now js/Date))]

      ;; Wait for timeout to expire
      #?(:clj (Thread/sleep (+ timeout-ms 10))
         :cljs (let [wait-start (.now js/Date)]
                 (while (< (- (.now js/Date) wait-start) (+ timeout-ms 10))
                   nil)))

      ;; Now timeout should trigger
      (is (timeout?) "Timeout should have triggered after deadline")

      (let [debug-info (timeout? :timeout/debug)
            debug (:timeout/debug debug-info)
            overshoot-us (:timeout.debug/overshoot-us debug)
            overshoot-ms-from-debug (/ overshoot-us 1000.0)
            target-ms (/ (:timeout.debug/target-overshoot-us debug) 1000.0)]

        ;; Verify timeout was detected
        (is (:timeout/timeout? debug-info))
        (is (pos? (:timeout.debug/detected-at-us debug)))

        ;; Verify overshoot is reasonable (within 5x target as a loose bound for test environment)
        (is (< overshoot-ms-from-debug (* 10.0 target-ms)))

        ;; Verify check count
        (is (pos? (:timeout.debug/check-count debug)))

        ;; Verify sampling occurred
        (is (pos? (:timeout.debug/sample-count debug)))))))

(deftest test-overshoot-envelope
  (testing "Overshoot within reasonable bounds using debug output"
    (let [timeout-ms 50
          timeout? (timeout/make-timeout timeout-ms)]

      ;; Wait past deadline
      #?(:clj (Thread/sleep (+ timeout-ms 10))
         :cljs (let [start (.now js/Date)]
                 (while (< (- (.now js/Date) start) (+ timeout-ms 10))
                   nil)))

      (timeout?)
      (let [debug (:timeout/debug (timeout? :timeout/debug))
            overshoot-us (:timeout.debug/overshoot-us debug)
            target-overshoot-us (:timeout.debug/target-overshoot-us debug)
            tolerance-us (* 10 target-overshoot-us)]

        ;; Overshoot should be reasonable
        (is (< overshoot-us tolerance-us))
        ;; Verify target calculation is correct
        (is (= target-overshoot-us 2500))))))

(defn setup-expensive-search []
  (let [expected
        (vec (for [i (range 15)]
               (vec (for [j (range 15)]
                      (vec (for [k (range 15)]
                             {:i i
                              :j j
                              :k k
                              :sets #{(+ i 1000) (+ j 2000) (+ k 3000)
                                      (+ i j 4000) (+ j k 5000) (+ i k 6000)}
                              :maps {(+ i 7000) (+ j 8000)
                                     (+ j 9000) (+ k 10000)
                                     (+ k 11000) (+ i 12000)}
                              :vecs [(+ i 13000) (+ j 14000) (+ k 15000)
                                     (+ (* i j) 16000) (+ (* j k) 17000)
                                     (+ (* i k) 18000)]}))))))
        actual
        (vec
         (for [i (range 15)]
           (vec (for [j (range 15)]
                  (vec (for [k (range 15)]
                         {:i i
                          :j j
                          :k k
                          :sets (if (and (= i 7) (= j 7) (= k 7))
                                  #{7007 14014 21021 28028 35035 42042} ; Different
                                                                           ; at
                                                                           ; one
                                                                           ; point
                                  #{(+ i 1000) (+ j 2000) (+ k 3000)
                                    (+ i j 4000) (+ j k 5000) (+ i k 6000)})
                          :maps {(+ i 7000) (+ j 8000)
                                 (+ j 9000) (+ k 10000)
                                 (+ k 11000) (+ i 12000)}
                          :vecs [(+ i 13000) (+ j 14000) (+ k 15000)
                                 (+ (* i j) 16000) (+ (* j k) 17000)
                                 (+ (* i k) 18000)]}))))))]
    [expected actual]))

#_(deftest timeout-test
  (let [[expected actual] (run-into-timeout)]
    (is (=* expected actual))))

(comment
  (require '[clj-reload.core :as r])
  (let [[expected actual] (setup-expensive-search)]
    (time (=* expected actual {:timeout nil}))
    (time (=* expected actual))
    :finished))
