(ns me.lomin.sinho.diff-test
  (:require
   [clojure.test :as t :refer [deftest is]]
   [me.lomin.sinho.diff :as diff]))

(deftest sort-paths-test
  (is (= [[[[:m-val #{3}]] [[:m-val #{1 2}]]]
          [[[:m-key #{3}] [:set 3]] [[:set 1]]]]
         (sort diff/compare-paths
               (list [[[:m-key #{3}] [:set 3]] [[:set 1]]]
                     [[[:m-val #{3}]] [[:m-val #{1 2}]]]))))

  (is (= [[[[:m-val #{3}] [:set 4]] [[:m-val #{1 2}] [:set 3]]]
          [[[:m-key #{3}] [:set 3]] [[:m-key #{1 2}] [:set 1]]]]
         (sort diff/compare-paths
               (list [[[:m-key #{3}] [:set 3]] [[:m-key #{1 2}] [:set 1]]]
                     [[[:m-val #{3}] [:set 4]] [[:m-val #{1 2}] [:set 3]]]))))

  (is (= [[[[::diff/nil]] [[:index 1]]]
          [[[::diff/nil]] [[:index 2]]]]
         (sort diff/compare-paths
               (list [[[::diff/nil]] [[:index 1]]]
                     [[[::diff/nil]] [[:index 2]]]))
         (sort diff/compare-paths
               (list [[[::diff/nil]] [[:index 2]]]
                     [[[::diff/nil]] [[:index 1]]])))))

(deftest grow-path-tree-test
  (is (= [::diff/node [] [[::diff/node [:m-val #{3}] [[::diff/leaf [[:m-val #{1 2}]]]]]]]
         (diff/grow-path-tree diff/root-node
                              [[[:m-val #{3}]] [[:m-val #{1 2}]]])))

  (is (= [::diff/node [] [[::diff/node [:m-val #{3}] [[::diff/leaf [[:m-val #{1 2}]]]]]
                          [::diff/node [:m-key #{3}] [[::diff/node [:set 3] [[::diff/leaf [[:set 1]]]]]]]]]
         (diff/grow-path-tree [::diff/node [] [[::diff/node [:m-val #{3}] [[::diff/leaf [[:m-val #{1 2}]]]]]]]
                              [[[:m-key #{3}] [:set 3]] [[:set 1]]])))

  (is (= [::diff/node []
          [[::diff/node [:m-val #{3}]
            [[::diff/leaf [[:m-val #{1 2}]]]]]
           [::diff/node [:m-key #{3}]
            [[::diff/node [:set 3]
              [[::diff/node [:test :me]
                [[::diff/leaf [[:set 1]]]]]]]]]]]
         (diff/grow-path-tree [::diff/node [] [[::diff/node [:m-val #{3}] [[::diff/leaf [[:m-val #{1 2}]]]]]]]
                              [[[:m-key #{3}] [:set 3] [:test :me]] [[:set 1]]])))

  (is (= [::diff/node []
          [[::diff/node [::diff/nil]
            [[::diff/leaf [[:index 1]]]]]]]
         (diff/grow-path-tree diff/root-node
                              [[[::diff/nil]] [[:index 1]]])))

  (is (= [::diff/node []
          [[::diff/node [::diff/nil]
            [[::diff/leaf [[:index 1]]]
             [::diff/leaf [[:index 2]]]]]]]
         (diff/grow-path-tree [::diff/node []
                               [[::diff/node [::diff/nil]
                                 [[::diff/leaf [[:index 1]]]]]]]
                              [[[::diff/nil]] [[:index 2]]]))))