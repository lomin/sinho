(ns me.lomin.sinho.output-test
  (:require [clojure.test :refer :all]
            [me.lomin.sinho.matcher :refer [=*]]))

(defn with-stdout [x]
  (prn x)
  x)

(deftest ^:kaocha/pending diff-output-test
  (testing "check that it is only printed once to stdout"
    (is (=* {#{1 2 4} :a}
            (with-stdout {#{1 2 3} :a}))
        "expected failure"))

  (is (=* {{2 2} :a}
          {{1 2 3 4} :a})
      "expected failure")

  (is (=* {#{3} #{1 2 4}}
          {#{1 2} #{1 2 3}})
      "expected failure")

  (is (=* [#{1 2} #{3 4}]
          [#{1} #{3}])
      "expected failure")

  (is (=* {{:b 1 :c 2} :a}
          {{:c 2} :a})
      "expected failure"))

(defn query-twitter-for [user]
  {:name           "Steven Collins"
   :twitter        "@lomin"
   :following      169
   :followers      122
   :person-of-note {{:name     "Borussia Dortmund"
                     :twitter  "@BlackYellow"
                     :folowing 146
                     :folowers 4579000}   :favorite
                    {:name      "FC Bayern English"
                     :twitter   "@FCBayernEN"
                     :following 49
                     :followers 11000000} :blocked}})

(deftest blocks-all-evil-test
  (is (=*
       {:twitter        "@lomin"
        :person-of-note {{:twitter "@FCBayernEN"} :blocked}}
       (query-twitter-for "@lomin"))))

(deftest ^:kaocha/pending blocks-all-evil-wrong-test
  (is (=*
       {:twitter        "@lomin"
        :person-of-note {{:instagram "@FCBayernEN"} :blocked}}
       (query-twitter-for "@lomin"))
      "expected failure"))
