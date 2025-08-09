(ns me.lomin.sinho.property-test
  (:require [clojure.test :refer :all]
            [me.lomin.sinho.matcher :refer [=*] :as matcher]
            [lambdaisland.deep-diff2.diff-impl :refer [->Insertion left-undiff] :as diff2]
            [clojure.test.check.clojure-test :as test]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(def containers (fn [inner-gen]
                  (gen/one-of [(gen/list inner-gen)
                               (gen/vector inner-gen)
                               (gen/set inner-gen)
                               (gen/map inner-gen inner-gen)])))

(def scalars (gen/frequency [[10 gen/simple-type-printable-equatable]
                             [1 (gen/return nil)]]))

(defn insertion? [x]
  (= (type x) (type (->Insertion nil))))

(def property-failure nil)
(def end-2-end-generative-test nil)
(test/defspec end-2-end-generative-test
  {:num-tests 100
   ;:seed 1599306615414
   }
  (prop/for-all [left (gen/recursive-gen containers scalars) right
                 (gen/recursive-gen containers scalars)]
                (let [d (=* left right {:timeout 100})]
                  (or (= d :timeout)
                      (= d left)
                      (insertion? d)
                      (= left (left-undiff d))
                      (and (def property-failure
                             [left right d {:timeout 100}])
                           false)))))

(def timeout-failure nil)
(def timeout-test-test nil)
(test/defspec timeout-test-test
  {:num-tests 100}
  (prop/for-all [timeout     (gen/no-shrink (gen/choose 10 500))
                 left        (gen/no-shrink (gen/recursive-gen containers scalars))
                 right       (gen/no-shrink (gen/recursive-gen containers scalars))]
                (let [start-time        (. System (currentTimeMillis))
                      d                 (=* left right {:timeout     timeout})
                      end-time          (. System (currentTimeMillis))
                      duration          (- end-time start-time)
                      accepted-duration (* timeout 2)]
                  (or (< duration accepted-duration)
                      (and (def timeout-failure [duration accepted-duration timeout d])
                           false)))))