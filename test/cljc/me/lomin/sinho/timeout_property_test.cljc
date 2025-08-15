(ns me.lomin.sinho.timeout-property-test
  "Property-based tests for timeout functionality that work on both CLJ and CLJS."
  (:require [clojure.test :refer [deftest testing is]]
            [me.lomin.sinho.timeout :as timeout]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defspec property-debug-consistency
  50
  (prop/for-all [timeout-ms (gen/choose 50 500)]
                (let [timeout? (timeout/make-timeout timeout-ms)
                      _ (timeout?)
                      debug-info (timeout? :timeout/debug)
                      debug (:timeout/debug debug-info)]
                  (and
                   ;; Debug counters should be consistent
                   (>= (:timeout.debug/check-count debug) 1)
                   (>= (:timeout.debug/sample-count debug) 1)
                   (<= (:timeout.debug/sample-count debug) (:timeout.debug/check-count debug))
                   ;; Statistics should be reasonable
                   (pos? (:timeout.debug/mean-step-us debug))
                   (>= (:timeout.debug/var-step-us debug) 0.0)
                   (pos? (:timeout.debug/decayed-max-us debug))
                   (pos? (:timeout.debug/effective-step-us debug))
                   ;; Stride should be within bounds
                   (>= (:timeout.debug/stride-final debug) 1)
                   (<= (:timeout.debug/stride-final debug) (bit-shift-left 1 20))
                   ;; Timestamps should be ordered correctly
                   (<= (:timeout.debug/start-us debug) (:timeout.debug/now-us debug))
                   (= (:timeout.debug/deadline-us debug)
                      (+ (:timeout.debug/start-us debug) (* 1000 timeout-ms)))))))

(defspec property-adaptive-efficiency
  50
  (prop/for-all [timeout-ms (gen/choose 500 2000)
                 num-calls (gen/choose 50 200)]
                (let [timeout? (timeout/make-timeout timeout-ms)]
                  (dotimes [_ num-calls] (timeout?))
                  (let [debug (:timeout/debug (timeout? :timeout/debug))
                        efficiency (/ (:timeout.debug/sample-count debug)
                                      (:timeout.debug/check-count debug))]
                    ;; Efficiency should improve with more calls
                    (< efficiency 0.5)))))

(defspec property-no-false-positives
  50
  (prop/for-all [timeout-ms (gen/choose 100 1000)
                 num-calls (gen/choose 10 100)]
                (let [timeout? (timeout/make-timeout timeout-ms)
                      ;; Make calls quickly (should finish before timeout)
                      results (doall (repeatedly num-calls timeout?))]
                  ;; All results should be false (no timeout)
                  (every? false? results))))