(ns me.lomin.sinho.example-matcher-combinator-test
  (:require
   [clojure.test :as t :refer [deftest is]]
   [me.lomin.sinho.matcher-combinator :refer [=*]]))

(defn query-twitter-for [_user] ; fake implementation
  {:name "Steven Collins"
   :twitter "@lomin"
   :following 169
   :followers 122
   :person-of-note {{:name "Borussia Dortmund"
                     :twitter "@BlackYellow"
                     :folowing 146
                     :folowers 4579000} :favorite
                    {:name "FC Bayern English"
                     :twitter "@FCBayernEN"
                     :following 49
                     :followers 11000000} :blocked}})

(deftest blocks-all-evil-test-mc
  (is (match? (=* {:twitter "@lomin"
                   :person-of-note {{:twitter "@FCBayernEN"} :blocked}})
              (query-twitter-for "@lomin"))))

#_(deftest failing-blocks-all-evil-test-mc
    (is (match? (=* {:twitter "@lomin"
                     :person-of-note {{:instagram "@FCBayernEN"} :blocked}})
                (query-twitter-for "@lomin"))))