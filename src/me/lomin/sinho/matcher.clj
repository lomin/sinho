(ns me.lomin.sinho.matcher
  (:require
   [clojure.test :as clojure-test]
   [com.rpl.specter :as s]
   [kaocha.report :as report]
   [me.lomin.sinho.a-star :as a-star]
   [me.lomin.sinho.search :as search]
   [me.lomin.sinho.diff :as diff]))

(defmethod clojure-test/assert-expr '=*
  [msg form]
  (let [[pred expected actual] form]
    `(let [result# (~pred ~expected ~actual)]
       (clojure-test/do-report {:type (if (= ~expected result#) :pass :fail)
                                :message ~msg
                                :expected '~form
                                :actual result#})
       result#)))


(defmethod kaocha.report/print-expr '=* [m]
  (report/print-expression m))

(def path-internals? #{::push ::pop})

(defn equality-partition [[a b] _]
  (or (cond
        (= a b) :atom
        (sequential? a) (when (sequential? b) :sequential)
        (sequential? b) :default
        (map? a) (when (map? b) :map)
        (map? b) :default
        (set? a) (when (set? b) :set)
        (set? b) :default
        :else :atom)
      :default))

(defmulti children equality-partition)

(defn heuristic-dispatch [[a :as comparison]]
  (if (path-internals? a)
    0
    (equality-partition comparison nil)))

(defmulti heuristic heuristic-dispatch)

(defn atom-count-seq [x] (::count-seq (meta x)))
(defn atom-count [x] (reduce + 1 (atom-count-seq x)))

(defmethod heuristic 0 [_]
  0)

(defmethod heuristic :default [comparison]
  (atom-count (first comparison)))

(defmethod heuristic :atom [[left right]]
  (if (= left right) 0 1))

(defmethod heuristic :sequential [[left-xs right-xs]]
  (let [left-count (count left-xs)
        right-count (count right-xs)
        b-xs (if (<= left-count right-count)
               right-xs
               left-xs)
        diff-count (abs (- left-count right-count))]
    (transduce (take diff-count) + (sort (atom-count-seq b-xs)))))

(defn heuristic:set|map [[left right]]
  (if (<= (count left) (count right))
    0
    (let [diff-count (- (count left) (count right))]
      (transduce (take diff-count) + (sort (atom-count-seq left))))))

(defmethod heuristic :set [comparison]
  (heuristic:set|map comparison))

(defmethod heuristic :map [comparison]
  (heuristic:set|map comparison))

(defn add-diff [node left]
  (-> node
      (update :diffs conj [(:left-path node) (:right-path node)])
      (a-star/inc-costs (atom-count left))))

(defmethod children :default [[left] node]
  (list (add-diff node left)))

(defmethod children :atom [[left right] node]
  (list (cond-> node
          (not= left right) (add-diff left))))

(defmacro stack-updates [path-0 values-0 & [_ pred path-1 values-1]]
  `(if ~pred
     [[::pop] ~values-1 [::push ~path-1]
      [::pop] ~values-0 [::push ~path-0]]
     [[::pop] ~values-0 [::push ~path-0]]))

(defn seq:stack-updates-default [left-index right-index left right]
  (stack-updates [[:index left-index] [:index right-index]]
                 [left right]))

(defn seq:dis [xs] (with-meta (rest xs) (update (meta xs) ::count-seq rest)))

(defn seq:stack-updates [[_ index xs] xs-f]
  (let [xs* (xs-f xs)]
    (vary-meta xs* assoc ::index (inc index))))

(defn seq:child-default [node
                         [left left-index :as left-indexed]
                         [right right-index :as right-indexed]]
  (-> node
      (update :stack conj [(seq:stack-updates left-indexed seq:dis)
                           (seq:stack-updates right-indexed seq:dis)])
      (update :stack into (seq:stack-updates-default left-index right-index left right))))

(defn seq:child-delete-first-element [node
                                      [left left-index :as left-indexed]
                                      [_ _ right-seq]]
  (-> node
      (update :stack conj [(seq:stack-updates left-indexed seq:dis)
                           right-seq])
      (update :diffs conj [(conj (:left-path node) [:index left-index])
                           (conj (:right-path node) [:index -1 :nil])])
      (a-star/inc-costs (atom-count left))))

(defn seq:child-add-first-element [node
                                   [_ left-index :as left-indexed]
                                   [right right-index :as right-indexed]]
  (-> node
      (update :stack conj [(seq:stack-updates left-indexed identity)
                           (seq:stack-updates right-indexed seq:dis)])
      (update :diffs conj [(conj (:left-path node) [:index left-index :before])
                           (conj (:right-path node) [:index right-index])])
      (a-star/inc-costs (atom-count right))))

(defn seq:first-or-absent [xs]
  (if (seq xs) (first xs) ::diff/nil))

(defn seq:meta-index [xs]
  (::index (meta xs) 0))

(defmethod children :sequential [comparison node]
  (let [[[left :as left-indexed]
         [right :as right-indexed]]
        (map (juxt seq:first-or-absent seq:meta-index identity) comparison)]
    (if (and (= left ::diff/nil) (= right ::diff/nil))
      (list node)
      (cond-> (list)
        (and (not= left ::diff/nil) (not= right ::diff/nil))
        (conj (seq:child-default node left-indexed right-indexed))
        (not= left ::diff/nil)
        (conj (seq:child-delete-first-element node left-indexed right-indexed))
        (not= right ::diff/nil)
        (conj (seq:child-add-first-element node left-indexed right-indexed))))))

(defn set|map:ensure-same-length-as [xs compare-xs nil-value]
  (cond-> xs (< (count xs) (count compare-xs)) (conj nil-value)))

(defn set|map:children
  [set|map:stack-updates dis nil-value [left right] node]
  (if (empty? left)
    (list node)
    (let [l (first left)
          left-1 (dis left l)]
      (for [r (-> right (set|map:ensure-same-length-as left nil-value))]
        (cond-> node
          (seq left-1) (update :stack conj [left-1 (dis right r)])
          :always (update :stack into (set|map:stack-updates l r)))))))

(defn set:dis [s x] (vary-meta (disj s x) update ::count-seq rest))

(defn set:stack-updates [left right]
  (stack-updates [[:set left] [:set right]] [left right]))

(defmethod children :set
  [comparison node]
  (set|map:children set:stack-updates
                    set:dis ::diff/nil comparison node))

(defn map:dis [m [k]] (vary-meta (dissoc m k) update ::count-seq rest))

(defn map:stack-updates [[left-key left-value] [right-key right-value]]
  (stack-updates [[:m-key left-key] [:m-key right-key]]
                 [left-key right-key]
                 :when (and (not= ::diff/nil left-key)
                            (not= ::diff/nil right-key))
                 [[:m-val left-key] [:m-val right-key]]
                 [left-value right-value]))

(defmethod children :map
  [comparison node]
  (set|map:children map:stack-updates
                    map:dis [::diff/nil ::diff/nil] comparison node))

(defn push-path [node [left-path right-path]]
  (-> node
      (update :left-path conj left-path)
      (update :right-path conj right-path)
      (update :stack pop)))

(defn pop-path [node]
  (-> node
      (update :left-path pop)
      (update :right-path pop)
      (update :stack pop)))

(defn update-path [node]
  (loop [{stack :stack :as p} node]
    (if-let [[k v] (peek stack)]
      (condp = k
        ::push (recur (push-path p v))
        ::pop (recur (pop-path p))
        p)
      p)))

(def meta-count-xf
  (map (comp inc (partial apply +) #(::count-seq % '(0)) meta)))

(defn add-count-meta [x]
  (let [xs (if (map? x) (mapcat seq x) (seq x))
        xf (cond-> meta-count-xf
             (map? x) (comp (partition-all 2)
                            (map (partial apply +))))]
    (as-> x $
      (->> xs
           (into [] xf)
           (vary-meta $ assoc ::count-seq)))))

(defn do-prepare [x]
  (if (map-entry? x)
    ;; map-entries are unable to hold meta-data, so continue as vector
    (vec x)
    (add-count-meta x)))

(def coll-walker+meta-nav
  (s/recursive-path
   [] p
   (s/if-path coll?
              (s/if-path map-entry?
                         (s/stay-then-continue p)
                         (s/continue-then-stay
                          [s/ALL-WITH-META p])))))

(defn prepare [x]
  (s/transform coll-walker+meta-nav do-prepare x))

(defrecord EqualStarNode [a-star:back+forward-costs a-star:best-costs
                          a-star:costs a-star:priority a-star:seen stack]
  a-star/AStar
  (get-costs [_] a-star:costs)
  (get-best-costs [_] a-star:best-costs)
  (get-back+forward-costs [_] a-star:back+forward-costs)
  (seen [_] a-star:seen)
  (forward-costs [_] (transduce (map heuristic) + stack))
  (a-star-identity [_] stack)
  (goal? [_] (empty? stack))
  search/SearchableNode
  (children [this]
    (when-let [comparison (peek stack)]
      (children comparison (update this :stack pop))))
  (priority [_] a-star:priority)
  (stop [{:keys [diffs] :as this} _]
    (if (<= (deref a-star:best-costs) a-star:back+forward-costs)
      this
      (when-let [result (when (a-star/goal? this)
                          (cond-> this (empty? diffs) (reduced)))]
        (vswap! a-star:best-costs min a-star:back+forward-costs)
        result)))
  (combine [this _other] this))

(defn equal-star-search-config [left right]
  (let [left* (prepare left)
        right* (prepare right)]
    (-> {:source     [left* right*]
         :stack      (list [left* right*])
         :diffs      ()
         :left-path  []
         :right-path []}
        (map->EqualStarNode)
        (a-star/init {:search-xf (a-star/with-xform (map update-path))}))))

(defn =*
  ([a b] (=* a b nil))
  ([a b options]
   (as-> (equal-star-search-config a b) $
     (search/search (merge $ options))
     (if (seq (:stack $))
       :timeout
       (diff/diff (:diffs $) (:source $))))))

