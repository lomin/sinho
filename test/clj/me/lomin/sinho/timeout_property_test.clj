(ns me.lomin.sinho.timeout-property-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [me.lomin.sinho.timeout :as timeout]))

;; ----- Property-Based Tests (CLJ only) -----

(defspec property-timeout-eventually-fires
  200 ; Increased to statistically significant sample size
  (prop/for-all [timeout-ms (gen/choose 10 100)]
                (let [timeout? (timeout/make-timeout timeout-ms)]
                  (Thread/sleep (+ timeout-ms 20)) ; Wait past deadline
                  (timeout?)))) ; Should return true

(defspec property-no-false-positives
  200 ; Increased to statistically significant sample size
  (prop/for-all [timeout-ms (gen/choose 100 1000)
                 calls (gen/choose 1 50)]
                (let [timeout? (timeout/make-timeout timeout-ms)]
                  (dotimes [_ calls] (timeout?))
                  (Thread/sleep 10) ; Small delay, but well under timeout
                  (not (timeout?))))) ; Should not timeout

(defspec property-adaptive-efficiency
  100 ; Increased to statistically significant sample size
  (prop/for-all [timeout-ms (gen/choose 200 1000)
                 num-calls (gen/choose 20 100)]
                (let [timeout? (timeout/make-timeout timeout-ms)]
                  (dotimes [_ num-calls] (timeout?))
                  (let [debug (:timeout/debug (timeout? :timeout/debug))
                        efficiency (/ (:timeout.debug/sample-count debug)
                                      (:timeout.debug/check-count debug))]
        ;; Efficiency should improve with more calls (lower sampling ratio)
                    (< efficiency 0.5))))) ; Should sample less than 50% of the time

(defspec property-debug-consistency
  150 ; Increased to statistically significant sample size
  (prop/for-all [timeout-ms (gen/choose 50 500)]
                (let [timeout? (timeout/make-timeout timeout-ms)
                      _ (timeout?) ; Make at least one call
                      debug-info (timeout? :timeout/debug)
                      debug (get debug-info :timeout/debug)]
                  (and
        ;; Debug counters should be consistent
                   (>= (get debug :timeout.debug/check-count) 1)
                   (>= (get debug :timeout.debug/sample-count) 1)
                   (<= (get debug :timeout.debug/sample-count) (get debug :timeout.debug/check-count))
        ;; Statistics should be reasonable
                   (pos? (get debug :timeout.debug/mean-step-us))
                   (>= (get debug :timeout.debug/var-step-us) 0.0)
                   (pos? (get debug :timeout.debug/decayed-max-us))
                   (pos? (get debug :timeout.debug/effective-step-us))
        ;; Stride should be within bounds
                   (>= (get debug :timeout.debug/stride-final) 1)
                   (<= (get debug :timeout.debug/stride-final) (bit-shift-left 1 20))
        ;; Timestamps should be ordered correctly
                   (<= (get debug :timeout.debug/start-us) (get debug :timeout.debug/now-us))
                   (= (get debug :timeout.debug/deadline-us)
                      (+ (get debug :timeout.debug/start-us) (* 1000 timeout-ms)))))))