(ns me.lomin.sinho.diff
  (:require [com.rpl.specter :as s]
            [clojure.walk :as walk]
            [arrangement.core :refer [rank]]
            [lambdaisland.deep-diff2.diff-impl :refer [->Mismatch
                                                       ->Deletion
                                                       ->Insertion]])
  #?(:cljs (:require [com.rpl.specter.navs :as snavs])))

;; Extend Specter's InsertBeforeIndex protocol to support Subvec in CLJS
#?(:cljs
   (extend-protocol snavs/InsertBeforeIndex
     cljs.core/Subvec
     (insert-before-idx [subvec idx val]
       ;; Implement same logic as PersistentVector but ensure we get proper vectors
       (let [subvec-as-vec (vec subvec)
             front (vec (take idx subvec-as-vec))
             back (vec (drop idx subvec-as-vec))]
         (into (conj front val) back)))))

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
             (s/filterer (fn [x]
                           #?(:clj (instance? java.lang.Comparable x)
                              :cljs (or (number? x) (string? x) (set? x) (keyword? x)))))
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

(defn grow-path-tree
  " Given a `left-source` and a `right-source`, a path-tree is a representation
   of the transformations to the left-source and the order that these transformations
   should be applied to the `left-source` to fulfill the property
   `(=* left-source right-source)`. A path-tree can be transformed by
   `path-tree->diff-transformer` into a transform function for `specter/multi-transform`.

   A path-tree contains nodes. Nodes that have no children are called leafs.
   Nodes represent how to iteratively navigate to a substructure of the `left-source` and
   leafs additionally represent that the substructure to which its is navigating to
   needs some kind of transformation in order to to fulfill the property
   `(=* left-source right-source)`. What kind of transformation that is, is no
   responsibility of the path-tree.

  This function `grow-path-tree` grows the path-tree one piece. In order to grow a
  valid path-tree, i.e. a path-tree that can be converted in a diff-transformer that
  will produce a valid diff, it is important that the first call of `grow-path-tree`
  must be with `root-node` as `tree`. Additionally, if one reduces over a sequence of
  `[left-path right-path]` pairs, the order of the sequence matters, because the tree
  grows from the leafs to the root."
  [tree [left-path right-path]]
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
     :before (s/if-path (fn [x]
                          #?(:clj (instance? clojure.lang.Cons x)
                             :cljs (and (seq? x) (not (vector? x)))))
                        ;; If it's a Cons, convert to list first, then apply before-index
                        [(s/view #(apply list %)) (s/before-index index)]
                        (s/before-index index))
     :nil (s/view (constantly ::nil)))))

(def navs
  {:set #(s/set-elem %)
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

(defn diff
  "Returns a diff of `left-source` and `right-source`.
   `paths` is a sequence of differences encoded as paths between `left-source` and
   `right-source`. It is not possible to reduce over all the paths and transform the
   substructure the path points to directly, because consecutive paths could be
   invalidated by the transformation. Instead, the transformations need to happen
   in a single pass, beginning with the deepest substructure and according to the order
   of the comparator `compare-paths`. The transformation in a single pass can be achieved
   by transforming `paths` - the sequence of differences - into a path-tree, i.e. a tree of
   differences, where the same subpaths are shared and only leafs of the tree differ."
  [paths [left-source right-source]]
  (as-> paths $
    (sort compare-paths $)
    (reduce grow-path-tree root-node $)
    (path-tree->diff-transformer right-source $)
    (s/multi-transform $ left-source)))