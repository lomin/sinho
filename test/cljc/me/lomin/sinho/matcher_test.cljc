(ns me.lomin.sinho.matcher-test
  (:require [clojure.test :as t :refer [deftest is testing are]]
            [me.lomin.sinho.matcher :as matcher]
            [com.rpl.specter :as s]
            [lambdaisland.deep-diff2.diff-impl :refer [->Mismatch ->Deletion ->Insertion] :as diff2]
            [arrangement.core :refer [rank]]
            [me.lomin.sinho.diff :as diff]))

(defn =* [a b & options]
  (matcher/to-diff2 (apply matcher/=* a b options)))

;; ── v3 coinductive engine internal tests ────────────────────────
;; These replace the v2.2 A*-based solve-test. They verify that the
;; coinductive engine produces correct diff paths for representative
;; inputs. The format differs from v2.2 (diff paths vs solve pairs)
;; because v3 uses structural walk instead of A* search.

(deftest coinductive-diff-paths-test
  (testing "sets - no diff when subset"
    (is (empty? (matcher/compute-diff-paths
                 (matcher/prepare #{1})
                 (matcher/prepare #{1}))))
    (is (empty? (matcher/compute-diff-paths
                 (matcher/prepare #{1})
                 (matcher/prepare #{1 2})))))

  (testing "sets - diff when superset"
    (is (seq (matcher/compute-diff-paths
              (matcher/prepare #{1 2})
              (matcher/prepare #{1})))))

  (testing "sequences - no diff when equal"
    (is (empty? (matcher/compute-diff-paths
                 (matcher/prepare [1 2])
                 (matcher/prepare [1 2])))))

  (testing "maps - no diff when subset"
    (is (empty? (matcher/compute-diff-paths
                 (matcher/prepare {:a 1})
                 (matcher/prepare {:a 1 :b 2})))))

  (testing "maps - diff when key missing"
    (is (seq (matcher/compute-diff-paths
              (matcher/prepare {:a {:b 1}})
              (matcher/prepare {:a {}})))))

  (testing "nested structures"
    (is (empty? (matcher/compute-diff-paths
                 (matcher/prepare [1 #{2}])
                 (matcher/prepare [1 #{2 3}])))))

  (testing "deeply nested"
    (is (empty? (matcher/compute-diff-paths
                 (matcher/prepare #{1 #{2 {:a 1}}})
                 (matcher/prepare #{1 4 #{2 3 {:a 1 :b 2}}}))))))

;; ── Verdict-level tests (must pass unchanged from v2.2) ─────────

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

;; ── Regression: greedy bipartite false-negative fix ─────────────
;; The greedy matcher could grab #{1 2 3 4} for #{1 2}, leaving #{3 4}
;; unable to find a zero-cost match. Exact bipartite matching on the
;; zero-cost subgraph fixes this.

(deftest bipartite-matching-regression-test
  (testing "set subset where greedy would fail"
    (is (= #{#{1 2} #{3 4}}
           (=* #{#{1 2} #{3 4}} #{#{1 2} #{1 2 3 4}}))))

  (testing "nested set subset"
    (is (= #{#{:a :b} #{:c :d}}
           (=* #{#{:a :b} #{:c :d}} #{#{:a :b :c} #{:c :d :e}}))))

  (testing "map with complex keys requiring bipartite"
    (is (= {#{1} #{1 2}}
           (=* {#{1} #{1 2}} {#{1 2} #{1 2 3}})))))

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

;; ── v3 cost computation test (replaces heuristic-test) ──────────

(deftest match-cost-test
  (testing "atoms"
    (is (= 0 (matcher/compute-cost (matcher/prepare 1) (matcher/prepare 1))))
    (is (= 1 (matcher/compute-cost (matcher/prepare 1) (matcher/prepare 2)))))

  (testing "sets - subset has zero cost"
    (is (= 0 (matcher/compute-cost (matcher/prepare #{1}) (matcher/prepare #{1 2}))))
    (is (= 0 (matcher/compute-cost (matcher/prepare #{1 2}) (matcher/prepare #{1 2})))))

  (testing "sets - superset has cost"
    (is (pos? (matcher/compute-cost (matcher/prepare #{1 2}) (matcher/prepare #{1})))))

  (testing "maps - subset has zero cost"
    (is (= 0 (matcher/compute-cost (matcher/prepare {1 2}) (matcher/prepare {1 2 3 4})))))

  (testing "maps - missing key has cost"
    (is (pos? (matcher/compute-cost (matcher/prepare {:a 1 :b 2}) (matcher/prepare {:a 1})))))

  (testing "sequences - equal has zero cost"
    (is (= 0 (matcher/compute-cost (matcher/prepare [1 2]) (matcher/prepare [1 2])))))

  (testing "sequences - different has cost"
    (is (pos? (matcher/compute-cost (matcher/prepare [1 2 3]) (matcher/prepare [1 2])))))

  ;; ── Exact-cost assertions for complex inputs (regression pins) ──
  ;; These pin exact cost values for representative complex inputs to
  ;; detect cost-function regressions that might not affect verdicts.
  ;; Values computed from the v3 coinductive engine and manually verified.
  (testing "exact costs for nested structures"
    (are [expected-cost left right]
         (= expected-cost (matcher/compute-cost (matcher/prepare left)
                                                (matcher/prepare right)))
      ;; nested sequence mismatch at leaf
      1   [1 [2 3]]               [1 [2 4]]
      ;; set superset: one missing element
      1   #{1 2 3}                #{1 2}
      ;; map missing two keys
      2   {:a 1 :b 2 :c 3}       {:a 1}
      ;; nested map value mismatch
      1   {:a {:b 1}}             {:a {:b 2}}
      ;; seq all elements missing
      3   [1 2 3]                 []
      ;; set of sets: inner elements missing
      2   #{#{1 2} #{3 4}}        #{#{1} #{3}}
      ;; deeply nested: set element missing through map+vec+set
      1   {:a [{:b #{1 2}}]}      {:a [{:b #{1}}]}
      ;; type mismatch: collection vs atom
      1   {:a [1 2]}              {:a 42}
      ;; seq extra expected elements (deletions cost 1 each)
      3   [1 2 3 4 5]             [1 2]
      ;; nested seq with leaf mismatches
      2   [[1 2] [3 4]]           [[1 3] [3 5]]
      ;; complex map: value mismatch + set missing element
      2   {:a 1 :b {:c [1 2 3]} :d #{4 5}}  {:a 1 :b {:c [1 2 4]} :d #{4}}
      ;; set with multiple missing elements
      3   #{1 2 3 4 5}            #{1 2}
      ;; map with set as key: inner element missing
      1   {#{1 2} :a}             {#{1} :a}
      ;; seq subset: extra in actual costs 0
      0   [1 2]                   [1 2 3 4 5]
      ;; map entry mismatch + missing key
      2   {:a 1 :b 2}             {:a 2}
      ;; nested list vs empty list
      2   '((1) (1 2 3))          '())))
