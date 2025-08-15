(ns me.lomin.sinho.example-test
  (:require
   [clojure.test :as t :refer [deftest is]]
   [matcher-combinators.test :refer [match?]]
   [me.lomin.sinho.matcher :refer [=*]]
   [me.lomin.sinho.matcher-combinator :as smc]))

(defn query-twitter-for [user] ; fake implementation
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

(deftest blocks-all-evil-test
  (is (=* {:twitter "@lomin"
           :person-of-note {{:twitter "@FCBayernEN"} :blocked}}
          (query-twitter-for "@lomin"))))

#_(deftest failing-blocks-all-evil-test
    (is (=* {:twitter "@lomin"
             :person-of-note {{:instagram "@FCBayernEN"} :blocked}}
            (query-twitter-for "@lomin"))))

;; Matcher-combinator integration tests

(deftest blocks-all-evil-test-mc
  (is (match? (smc/=* {:twitter "@lomin"
                       :person-of-note {{:twitter "@FCBayernEN"} :blocked}})
              (query-twitter-for "@lomin"))))

#_(deftest failing-blocks-all-evil-test-mc
    (is (match? (smc/=* {:twitter "@lomin"
                         :person-of-note {{:instagram "@FCBayernEN"} :blocked}})
                (query-twitter-for "@lomin"))))


#_(deftest timeout-test
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
    (is (=* expected actual {:timeout 100}))))