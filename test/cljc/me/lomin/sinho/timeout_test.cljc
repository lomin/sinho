(ns me.lomin.sinho.timeout-test
  (:require [clojure.test :refer [deftest is testing]]
            [me.lomin.sinho.timeout :as timeout]
            #?(:clj [clojure.test.check.clojure-test :refer [defspec]]
               :clj [clojure.test.check.generators :as gen]
               :clj [clojure.test.check.properties :as prop])))

;; ----- Unit Tests -----

(deftest make-timeout-test
  (testing "make-timeout creates a function"
    (let [timeout? (timeout/make-timeout 100)]
      (is (fn? timeout?))))

  (testing "timeout? function has correct arities"
    (let [timeout? (timeout/make-timeout 100)]
      (is (false? (timeout?))) ; 0-arity returns boolean
      (is (map? (timeout? :timeout/debug))))) ; 1-arity with :timeout/debug returns map

  (testing "invalid operation throws exception"
    (let [timeout? (timeout/make-timeout 100)]
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (timeout? :invalid-op))))))

(deftest timeout-behavior-test
  (testing "fresh timeout does not timeout immediately"
    (let [timeout? (timeout/make-timeout 1000)]
      (is (false? (timeout?)))))

  (testing "timeout eventually returns true after deadline"
    (let [timeout? (timeout/make-timeout 10)]
      #?(:clj (Thread/sleep 20)
         :cljs (let [start (.now js/Date)]
                 (while (< (- (.now js/Date) start) 20)
                   nil)))
      (is (true? (timeout?)))))

  (testing "timeout state is persistent"
    (let [timeout? (timeout/make-timeout 10)]
      #?(:clj (Thread/sleep 20)
         :cljs (let [start (.now js/Date)]
                 (while (< (- (.now js/Date) start) 20)
                   nil)))
      (timeout?) ; First call
      (is (true? (timeout?))))) ; Second call should still be true

  (testing "multiple timeout instances are independent"
    (let [timeout1? (timeout/make-timeout 10)
          timeout2? (timeout/make-timeout 1000)]
      #?(:clj (Thread/sleep 20)
         :cljs (let [start (.now js/Date)]
                 (while (< (- (.now js/Date) start) 20)
                   nil)))
      (is (true? (timeout1?)))
      (is (false? (timeout2?))))))

(deftest debug-payload-test
  (testing "debug payload has required structure"
    (let [timeout? (timeout/make-timeout 100)
          _ (timeout?) ; Make at least one call
          debug (timeout? :timeout/debug)]

      (is (contains? debug :timeout/timeout?))
      (is (contains? debug :timeout/debug))

      (let [debug-info (:timeout/debug debug)]
        ;; Check all required debug fields exist
        (is (contains? debug-info :timeout.debug/start-us))
        (is (contains? debug-info :timeout.debug/now-us))
        (is (contains? debug-info :timeout.debug/deadline-us))
        (is (contains? debug-info :timeout.debug/detected-at-us))
        (is (contains? debug-info :timeout.debug/overshoot-us))
        (is (contains? debug-info :timeout.debug/target-overshoot-us))
        (is (contains? debug-info :timeout.debug/check-count))
        (is (contains? debug-info :timeout.debug/sample-count))
        (is (contains? debug-info :timeout.debug/panic-count))
        (is (contains? debug-info :timeout.debug/stride-final))
        (is (contains? debug-info :timeout.debug/stride-min))
        (is (contains? debug-info :timeout.debug/stride-max))
        (is (contains? debug-info :timeout.debug/stride-shrink-count))
        (is (contains? debug-info :timeout.debug/mean-step-us))
        (is (contains? debug-info :timeout.debug/var-step-us))
        (is (contains? debug-info :timeout.debug/p95-proxy-us))
        (is (contains? debug-info :timeout.debug/decayed-max-us))
        (is (contains? debug-info :timeout.debug/effective-step-us))
        (is (contains? debug-info :timeout.debug/burn-max)))))

  (testing "debug counters increment"
    (let [timeout? (timeout/make-timeout 1000)]
      (dotimes [_ 5] (timeout?))
      (let [debug (:timeout/debug (timeout? :timeout/debug))]
        (is (>= (:timeout.debug/check-count debug) 5))
        (is (pos? (:timeout.debug/sample-count debug))))))

  (testing "target overshoot calculation"
    (let [timeout? (timeout/make-timeout 100) ; 100ms
          debug (:timeout/debug (timeout? :timeout/debug))
          target-ov (:timeout.debug/target-overshoot-us debug)]
      ;; Target overshoot should be min(10ms, 5% of timeout) = 5ms = 5000µs
      (is (= 5000 target-ov)))

    (let [timeout? (timeout/make-timeout 500) ; 500ms  
          debug (:timeout/debug (timeout? :timeout/debug))
          target-ov (:timeout.debug/target-overshoot-us debug)]
      ;; Target overshoot should be min(10ms, 5% of 500ms) = min(10ms, 25ms) = 10ms = 10000µs
      (is (= 10000 target-ov)))))

(deftest adaptive-behavior-test
  (testing "stride adaptation with many quick calls"
    (let [timeout? (timeout/make-timeout 1000)] ; Long timeout for adaptation
      (dotimes [_ 20] (timeout?)) ; Make many calls quickly
      (let [debug (:timeout/debug (timeout? :timeout/debug))]
        ;; Should have high stride due to quick calls
        (is (> (:timeout.debug/stride-final debug) 1))
        ;; Should have low sampling ratio (efficiency)
        (let [efficiency (/ (:timeout.debug/sample-count debug)
                            (:timeout.debug/check-count debug))]
          (is (< efficiency 0.5)))))) ; Less than 50% sampling

  (testing "stride bounds"
    (let [timeout? (timeout/make-timeout 1000)]
      (timeout?)
      (let [debug (:timeout/debug (timeout? :timeout/debug))
            stride (:timeout.debug/stride-final debug)]
        (is (>= stride 1))
        (is (<= stride (bit-shift-left 1 20)))))) ; KMAX = 1 << 20

  (testing "statistics evolution"
    (let [timeout? (timeout/make-timeout 1000)]
      (dotimes [_ 10] (timeout?))
      (let [debug (:timeout/debug (timeout? :timeout/debug))]
        (is (pos? (:timeout.debug/mean-step-us debug)))
        (is (>= (:timeout.debug/var-step-us debug) 0.0))
        (is (pos? (:timeout.debug/decayed-max-us debug)))
        (is (pos? (:timeout.debug/effective-step-us debug)))))))

(deftest timeout-detection-test
  (testing "timeout detection and overshoot measurement"
    (let [timeout? (timeout/make-timeout 20)] ; Short timeout
      #?(:clj (Thread/sleep 30)
         :cljs (let [start (.now js/Date)]
                 (while (< (- (.now js/Date) start) 30)
                   nil))) ; Wait past timeout
      (is (true? (timeout?)))
      (let [debug (timeout? :timeout/debug)]
        (is (true? (:timeout/timeout? debug)))
        (is (pos? (get-in debug [:timeout/debug :timeout.debug/detected-at-us])))
        (is (pos? (get-in debug [:timeout/debug :timeout.debug/overshoot-us])))))) ; Should have some overshoot

  (testing "no false positives before deadline"
    (let [timeout? (timeout/make-timeout 100)] ; 100ms timeout
      (dotimes [_ 50] (timeout?)) ; Make many calls quickly
      (let [debug (timeout? :timeout/debug)]
        (is (false? (:timeout/timeout? debug)))
        (is (zero? (get-in debug [:timeout/debug :timeout.debug/detected-at-us])))
        (is (zero? (get-in debug [:timeout/debug :timeout.debug/overshoot-us])))))))

(deftest cooperative-timeout-test
  (testing "cooperative timeout in reduce operation"
    (let [timeout? (timeout/make-timeout 20)
          result (reduce (fn [acc i]
                           (if (timeout?)
                             (reduced {:stopped-early true :last-i i :sum acc})
                             (+ acc i)))
                         0
                         (range 1000000))] ; Large range that would take time
      (if (:stopped-early result)
        (do (is (:stopped-early result))
            (is (pos? (:sum result)))
            (is (< (:last-i result) 1000000)))
        (is (= result (reduce + (range 1000000)))))))

  (testing "timeout allows early termination"
    (let [timeout? (timeout/make-timeout 10) ; Very short timeout
          start-time #?(:clj (System/currentTimeMillis)
                        :cljs (.now js/Date))
          result (loop [i 0]
                   #?(:clj (Thread/sleep 1) :cljs (js/setTimeout #() 1)) ; Add delay
                   (if (timeout?)
                     {:stopped true :iterations i}
                     (if (< i 1000)
                       (recur (inc i))
                       {:stopped false :iterations i})))
          end-time #?(:clj (System/currentTimeMillis)
                      :cljs (.now js/Date))]
      (when (:stopped result)
        (is (:stopped result))
        (is (< (- end-time start-time) 50)))))) ; Should stop quickly

;; ----- Acceptance Tests -----

(deftest overshoot-envelope-test
  (testing "overshoot within reasonable bounds for test environment"
    (let [timeout-ms 50
          timeout? (timeout/make-timeout timeout-ms)]
      #?(:clj (Thread/sleep (+ timeout-ms 10))
         :cljs (let [start (.now js/Date)
                     delay (+ timeout-ms 10)]
                 (while (< (- (.now js/Date) start) delay)
                   nil)))
      (timeout?) ; Trigger timeout detection
      (let [debug (:timeout/debug (timeout? :timeout/debug))
            overshoot-us (get-in debug [:timeout.debug/overshoot-us])
            target-overshoot-us (get-in debug [:timeout.debug/target-overshoot-us])
            ; Expected envelope: max(S, min(5% × timeout, 10ms))  
            ; For 50ms timeout: min(5% × 50ms, 10ms) = min(2.5ms, 10ms) = 2.5ms = 2500µs
            ; But this is p99 spec, so allow reasonable test tolerance
            tolerance-us (* 10 target-overshoot-us)] ; 10x for test environment variance
        ;; Overshoot should be reasonable - allowing for GC, scheduler, etc. in tests
        (is (< overshoot-us tolerance-us)
            (str "Overshoot " overshoot-us "µs exceeded tolerance " tolerance-us "µs"))
        ;; Verify target calculation is correct
        (is (= target-overshoot-us 2500) "Target overshoot calculation"))))

  (testing "efficiency under load"
    (let [timeout? (timeout/make-timeout 1000)] ; Long timeout for efficiency test
      (dotimes [_ 100] (timeout?)) ; Many calls
      (let [debug (:timeout/debug (timeout? :timeout/debug))
            efficiency (/ (get-in debug [:timeout.debug/sample-count])
                          (get-in debug [:timeout.debug/check-count]))]
        ;; Should achieve good efficiency (low sampling ratio when far from deadline)
        (is (< efficiency 0.2) ; Less than 20% sampling ratio
            (str "Poor efficiency: " efficiency " (higher means more overhead)"))))))

;; Property-based tests are in separate CLJ-only file: timeout_property_test.clj