(ns me.lomin.sinho.output-test
  (:require [clojure.test :refer :all]
            [me.lomin.sinho.matcher :refer [=*]]
            [me.lomin.sinho.matcher-combinator :as smc]))

(defn with-stdout [x]
  (prn x)
  x)

(deftest ^:kaocha/pending diff-output-test
  (testing "check that it is only printed once to stdout"
    (is (=* {#{1 2 4} :a}
            (with-stdout {#{1 2 3} :a}))
        "expected failure")
    (is (match? (smc/=* {#{1 2 4} :a})
                (with-stdout {#{1 2 3} :a}))
        "expected failure"))

  (is (=* {{2 2} :a}
          {{1 2 3 4} :a})
      "expected failure")
  (is (match? (smc/=* {{2 2} :a})
              {{1 2 3 4} :a})
      "expected failure")

  (is (=* {#{3} #{1 2 4}}
          {#{1 2} #{1 2 3}})
      "expected failure")
  (is (match? (smc/=* {#{3} #{1 2 4}})
              {#{1 2} #{1 2 3}})
      "expected failure")

  (is (=* [#{1 2} #{3 4}]
          [#{1} #{3}])
      "expected failure")
  (is (match? (smc/=* [#{1 2} #{3 4}])
              [#{1} #{3}])
      "expected failure")

  (is (=* {{:b 1 :c 2} :a}
          {{:c 2} :a})
      "expected failure")
  (is (match? (smc/=* {{:b 1 :c 2} :a}
                  {{:c 2} :a}))
      "expected failure"))