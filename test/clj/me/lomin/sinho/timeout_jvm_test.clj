(ns me.lomin.sinho.timeout-jvm-test
  "CLJ-only timeout tests for thread safety.
   
   This namespace contains tests that can only run on the JVM:
   Thread safety tests (Threads not available in CLJS)"
  (:require [clojure.test :refer [deftest testing is]]
            [me.lomin.sinho.timeout :as timeout]))

;; ============================================================================
;; Thread Safety Test (JVM Only)
;; ============================================================================

(deftest test-thread-safety-with-debug
  (testing "Thread safety with debug validation"
    (let [n-threads 3
          timeout-ms 100
          work-fn (fn [] (reduce + (range 20)))
          results (atom [])

          threads (for [i (range n-threads)]
                    (Thread.
                     (fn []
                       (let [timeout? (timeout/make-timeout timeout-ms)]
                         (loop [iterations 0]
                           (if (timeout?)
                             (let [debug-info (timeout? :timeout/debug)]
                               (swap! results conj {:thread i
                                                    :iterations iterations
                                                    :debug debug-info}))
                             (do (work-fn) (recur (inc iterations)))))))))]

      ;; Run threads
      (doseq [t threads] (.start t))
      (doseq [t threads] (.join t))

      (let [completed (count @results)]
        ;; Verify all threads completed
        (is (= completed n-threads))

        ;; Verify each thread's debug output is consistent
        (doseq [{:keys [thread iterations debug]} @results]
          (let [d (:timeout/debug debug)]
            (is (:timeout/timeout? debug))
            (is (pos? (:timeout.debug/detected-at-us d)))
            (is (= (inc iterations) (:timeout.debug/check-count d)))))))))