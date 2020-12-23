(ns me.lomin.sinho.example-test
  (:require [clojure.test :refer :all]
            [me.lomin.sinho.matcher :refer [=*]]))

(defn query-twitter-for [user] ; fake implementation
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
  (is (=* {:twitter        "@lomin"
           :person-of-note {{:twitter "@FCBayernEN"} :blocked}}
          (query-twitter-for "@lomin"))))

(deftest ^:kaocha/pending failing-blocks-all-evil-test
  (is (=* {:twitter        "@lomin"
           :person-of-note {{:instagram "@FCBayernEN"} :blocked}}
          (query-twitter-for "@lomin"))))