(ns me.lomin.sinho.matcher-test
  (:require [clojure.test :as t :refer [deftest is testing are]]
            [me.lomin.sinho.search :as search]
            [me.lomin.sinho.matcher :as matcher]
            [com.rpl.specter :as s]
            [lambdaisland.deep-diff2.diff-impl :refer [->Mismatch ->Deletion ->Insertion] :as diff2]
            [arrangement.core :refer [rank]]
            [me.lomin.sinho.diff :as diff]
            [me.lomin.sinho.a-star :as a-star]))

(defn =* [a b & options]
  (matcher/to-diff2 (apply matcher/=* a b options)))

(defn diff-paths [node]
  (:diffs node))

(defn solve [{[left-source right-source] :source :as node}]
  (vec (sort rank (map (fn [[left-path right-path]]
                         [(s/select-first (diff/path->navigators diff/navs left-path) left-source)
                          (s/select-first (diff/path->navigators diff/navs right-path) right-source)])
                       (:diffs node)))))

(defn search [search-config]
  (search/search (merge search-config {})))

(deftest solve-test

  (is (= [] (-> (matcher/equal-star-search-config #{1}
                                                  #{1})
                (search)
                (solve))))

  (is (= []
         (-> (matcher/equal-star-search-config #{1}
                                               #{1 2})
             (search)
             (solve))))

  (is (= [[2 ::diff/nil]]
         (-> (matcher/equal-star-search-config #{1 2}
                                               #{1})
             (search)
             (solve))))

  (is (= []
         (-> (matcher/equal-star-search-config [1 2]
                                               [1 2])
             (search)
             (solve))))

  (is (= [[[[:index 2 :before]] [[:index 2]]]
          [[[:index 1 :before]] [[:index 1]]]]
         (-> (matcher/equal-star-search-config [1]
                                               [1 2 3])
             (search)
             (diff-paths))))

  (is (= (list [[[:index 0]] [[:index 0]]])
         (-> (matcher/equal-star-search-config [nil 1] [0 1])
             (search)
             (diff-paths))))

  (is (= (list [[[:set 2]]
                [[:set :me.lomin.sinho.diff/nil]]])
         (-> (matcher/equal-star-search-config #{1 2} #{1})
             (search)
             (diff-paths))))

  (is (= '()
         (-> (matcher/equal-star-search-config #{1} #{1 2})
             (search)
             (diff-paths))))

  (is (= '()
         (-> (matcher/equal-star-search-config [1 [2 3]]
                                               [1 [2 3]])
             (search)
             (solve))))

  (is (= [[nil 3]]
         (-> (matcher/equal-star-search-config [1 2]
                                               [1 2 3])
             (search)
             (solve))))

  (is (= [[3 ::diff/nil]]
         (-> (matcher/equal-star-search-config [1 2 3]
                                               [1 2])
             (search)
             (solve))))

  (is (= []
         (-> (matcher/equal-star-search-config [1 #{2}]
                                               [1 #{2 3}])
             (search)
             (solve))))

  (is (= [[5 4]]
         (-> (matcher/equal-star-search-config #{1 5 #{2}}
                                               #{1 4 #{2 3}})
             (search)
             (solve))))

  (is (= [[3 ::diff/nil]
          [5 4]]
         (-> (matcher/equal-star-search-config #{1 5 #{2 3}}
                                               #{1 4 #{2}})
             (search)
             (solve))))

  (is (= []
         (-> (matcher/equal-star-search-config #{1 #{2 {:a 1}}}
                                               #{1 4 #{2 3 {:a 1 :b 2}}})
             (search)
             (solve))))

  (is (= [[{:a 1} 3]]
         (-> (matcher/equal-star-search-config #{1 #{2 {:a 1}}}
                                               #{1 4 #{2 3}})
             (search)
             (solve))))

  (is (= [[1 2]]
         (-> (matcher/equal-star-search-config {{:a 1} 3}
                                               {{:a 2} 3})
             (search)
             (solve))))

  (is (= [[:b ::diff/nil]]
         (-> (matcher/equal-star-search-config {:a {:b 1}}
                                               {:a {}})
             (search)
             (solve))))

  (is (= [[:e ::diff/nil]]
         (-> (matcher/equal-star-search-config {:a {:b 2 :c {:d 4 :e 5}}}
                                               {:a {:b 2 :c {:d 4}}})
             (search)
             (solve))))

  (is (= [[1 5]
          [4 6]]
         (-> (matcher/equal-star-search-config {#{1} #{2 3 4}}
                                               {#{5} #{2 3 6}})
             (search)
             (solve))))

  (is (= [[1 0]
          [3 4]]
         (-> (matcher/equal-star-search-config {#{1 #{2 3} 4} 5}
                                               {#{0 #{2 4} 4} 5})
             (search)
             (solve)))))

(deftest path-to-diff
  (is (= [1 (->Mismatch 2 3)]
         (=* [1 2] [1 3])
         (diff2/diff [1 2] [1 3])))

  (is (= [1 (->Insertion 2) (->Insertion 3)]
         (=* [1] [1 2 3])
         (diff2/diff [1] [1 2 3])))

  (is (= [(->Mismatch nil 0) 1]
         (=* [nil 1] [0 1])
         (diff2/diff [nil 1] [0 1])))

  (is (= [(->Deletion -1)]
         (=* [-1] '())
         (diff2/diff [-1] '())))

  (is (= #{1 (->Deletion 2)}
         (=* #{1 2} #{1})
         (diff2/diff #{1 2} #{1})))

  (is (= [1 (->Insertion 2)]
         (=* [1] [1 2])
         (diff2/diff [1] [1 2])))

  (testing "difference from diff2"
    (is (= #{1 (->Mismatch 2 3)}
           (=* #{1 2} #{1 3})))

    (is (= #{1 (->Deletion 2) (->Insertion 3)}
           (diff2/diff #{1 2} #{1 3}))))

  (is (= #{1 2}
         (=* #{1 2} #{1 2})
         (diff2/diff #{1 2} #{1 2})))

  (is (= {:a {(->Deletion :b) 1}}
         (=* {:a {:b 1}}
             {:a {}})
         (diff2/diff {:a {:b 1}}
                     {:a {}})))

  (is (= {{(->Deletion :b) 1
           :c 2} :a}
         (=* {{:b 1 :c 2} :a}
             {{:c 2} :a})))

  (is (= {(->Deletion {:b 1 :c 2}) :a
          (->Insertion {:c 2}) :a}
         (diff2/diff {{:b 1 :c 2} :a}
                     {{:c 2} :a})))

  (is (= [#{1} #{3}]
         (=* [#{1} #{3}]
             [#{1 2} #{3 4}])))

  (is (= [#{(->Deletion 2) 1}
          #{(->Deletion 4) 3}]
         (=* [#{1 2} #{3 4}]
             [#{1} #{3}])
         (diff2/diff [#{1 2} #{3 4}]
                     [#{1} #{3}])))

  (is (= {#{1} #{1 2}}
         (=* {#{1} #{1 2}}
             {#{1 2} #{1 2 3}})))

  (is (= {{#{1} #{3}} #{5}}
         (=* {{#{1} #{3}} #{5}}
             {{#{1 2} #{3 4}} #{5 6}})))

  (is (= {#{1 (->Deletion 2)} :a}
         (=* {#{1 2} :a}
             {#{1} :a})))

  (is (= {(->Deletion #{1 2}) :a
          (->Insertion #{1}) :a}
         (diff2/diff {#{1 2} :a}
                     {#{1} :a})))

  (is (= #{(->Deletion "")
           (->Deletion 0)}
         (=* #{"" 0} #{})
         (=* #{"" 0} #{} {:timeout 1000})))

  (is (= {0 (->Mismatch 0 1)}
         (=* {0 0}
             {0 1, -1 1}
             {})))

  (let [a [[:x 1 :y :z] [:x :y] :c :d]
        b [[:x :y :z] [:x 1 :y :z] :c :d]]
    (is (= [(->Insertion [:x :y :z])
            [:x 1 :y :z]
            (->Deletion [:x :y])
            :c :d]
           (diff2/diff a b)))
    (is (= [[:x (->Deletion 1) :y :z]
            [:x (->Insertion 1) :y (->Insertion :z)]
            :c
            :d]
           (=* a b))))

  (is (= [(->Deletion :b)
          :a
          (->Mismatch :x :y)]
         (=* [:b :a :x]
             [:a :y])))

  (is (= [(->Insertion :b)
          :a
          (->Mismatch :y :x)]
         (=* [:a :y]
             [:b :a :x])))

  (is (= {[1 (->Mismatch 2 3)] (->Mismatch :b :c)}
         (=* {#?(:clj (seq (to-array #{1 2}))
                 :cljs (seq (into-array #{1 2}))) :b}
             {#?(:clj (seq (to-array #{1 3}))
                 :cljs (seq (into-array #{1 3}))) :c})))

  (is (= [[:a (->Mismatch 1 2)]]
         (=* (seq {:a 1})
             (seq {:a 2})))))

(deftest prepare-test
  (is (= 22
         (matcher/atom-count
          (matcher/prepare {1 [4
                               5
                               {{1 2 3 4} {5 6 7 8}}
                               #{4 5 [6 7]}]}))))
  (is (= '(1 1 11 6)
         (matcher/atom-count-seq
          (matcher/prepare [4
                            5
                            {{1 2 3 4} {5 6 7 8}}
                            #{4 5 [6 7]}]))))

  (is (= (matcher/atom-count-seq
          (matcher/prepare [[[[:a 1] [:b 2]] [[:c 2] [:d 4]]]]))
         (matcher/atom-count-seq
          (matcher/prepare (seq {(seq {:a 1 :b 2}) (seq {:c 2 :d 4})}))))))

(defn calculate-back+forward-costs [left right]
  (a-star/calculate-back+forward-costs
   (:root-node (matcher/equal-star-search-config left right))))

(deftest heuristic-test
  (are [expected left right]
       (= expected (calculate-back+forward-costs left right))
    0 1 1
    1 1 2
    1 #{} {}
    4 '(1) (range 5)
    6 '((1) (1 2 3)) '()
    0 #{1} #{3 4}
    0 #{1 2} #{3 4}
    1 #{1 2} #{3}
    4 #{(range 5) (range 3)} #{3}
    0 {1 2} {3 4 5 6}
    2 {3 4 5 6} {1 2}
    7 {(range 5) 2 3 (range 10)} {1 2}
    1 '(1 :y :z) '(:y :z)))