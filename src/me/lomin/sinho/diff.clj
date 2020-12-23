(ns me.lomin.sinho.diff
  (:require [com.rpl.specter :as s]
            [clojure.walk :as walk]
            [arrangement.core :refer [rank]]
            [lambdaisland.deep-diff.diff :refer [->Mismatch
                                                 ->Deletion
                                                 ->Insertion]]))

(def root-node [::node #_::node #_or #_::leaf
                [#_[tag v :as path-segment]]
                [#_[:as children-nodes]]])
(def tag-nav (s/nthpath 1))
(def children-nav (s/nthpath 2))
(def noop-terminal-nav (s/terminal identity))
(defn terminal-nav [tag & args]
  (s/terminal #(conj % (into [tag] args))))

(def tag-ranking
  (into {}
        (map-indexed (fn [i k] [k i])
                     [:index :m-val :set :m-key ::nil nil])))

(defn comparable-vector [path]
  (s/select [s/ALL
             s/ALL
             (s/filterer (partial instance? java.lang.Comparable))
             (s/view (fn [[tag opt-arg]]
                       [(tag-ranking tag) opt-arg]))
             s/ALL]
            path))

(defn compare-paths [left-path right-path]
  (rank (comparable-vector left-path)
        (comparable-vector right-path)))

(defn upsert-navigator [inner-navigator path-segment]
  (let [path-segment-present?-nav [s/ALL
                                   tag-nav
                                   (s/pred= path-segment)]
        insert-path-segment-nav (terminal-nav ::node path-segment [])
        insert-path-segment-if-absent-nav (s/if-path path-segment-present?-nav
                                                     noop-terminal-nav
                                                     insert-path-segment-nav)
        insert-within-path-segment [s/ALL
                                    (s/selected? tag-nav (s/pred= path-segment))
                                    inner-navigator]]
    [children-nav
     (s/multi-path insert-path-segment-if-absent-nav
                   insert-within-path-segment)]))

(defn grow-path-tree [tree [left-path right-path]]
  (let [insert-leaf-child-nav [children-nav (terminal-nav ::leaf right-path)]]
    (s/multi-transform (reduce upsert-navigator
                               insert-leaf-child-nav
                               (reverse left-path))
                       tree)))

(defn seq-nav
  ([index] (s/nthpath index))
  ([index position & _]
   (case position
     :after s/AFTER-ELEM
     :before (s/before-index index)
     :nil (s/view (constantly ::nil)))))

(def navs {:set   #(s/set-elem %)
           :m-key #(s/map-key %)
           :m-val #(s/keypath %)
           :index seq-nav})

(defn path-segment->navigator [navigator-mapping [tag & args]]
  (if (= args '(::nil))
    (s/view (constantly ::nil))
    (apply (navigator-mapping tag) args)))

(defn node-diff-transformer [[_ path-segment children]]
  (if (seq path-segment)
    [(path-segment->navigator navs path-segment)
     (apply s/multi-path children)]
    (apply s/multi-path noop-terminal-nav children)))

(def none? #{::nil :com.rpl.specter.impl/NONE})

(defn path->navigators [navigator-mapping path]
  (mapv (partial path-segment->navigator navigator-mapping) path))

(defn leaf-diff-transformer [[_ right-path] right-source]
  (let [right (s/select-first (path->navigators navs right-path)
                              right-source)]
    (s/terminal (fn [left]
                  (cond
                    (none? left) (->Insertion right)
                    (none? right) (->Deletion left)
                    :else (->Mismatch left right))))))

(defn node-of? [t x] (and (seqable? x) (= (first x) t)))

(defn path-tree->diff-transformer [right-source selector-tree]
  (walk/postwalk #(cond (node-of? ::node %) (node-diff-transformer %)
                        (node-of? ::leaf %) (leaf-diff-transformer % right-source)
                        :else %)
                 selector-tree))

(defn diff [paths [left-source right-source]]
  (as-> paths $
        (sort compare-paths $)
        (reduce grow-path-tree root-node $)
        (path-tree->diff-transformer right-source $)
        (s/multi-transform $ left-source)))