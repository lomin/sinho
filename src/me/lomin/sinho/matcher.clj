(ns me.lomin.sinho.matcher
  (:require [clojure.test :as clojure-test]
            [kaocha.report]
            [kaocha.output :as output]
            [me.lomin.chatda.search :as search]
            [me.lomin.chatda.a-star :as a-star]
            [me.lomin.sinho.diff :as diff]
            [com.rpl.specter :as s]))

(defmethod clojure-test/assert-expr '=* [msg form]
  (let [[_ expected actual] form]
    (let [args (rest form)
          pred (first form)]
      `(let [values# (list ~@args)
             result# (~pred ~expected ~actual)]
         (if (= ~expected result#)
           (clojure-test/do-report {:type     :pass, :message ~msg,
                                    :expected '~form,
                                    :actual   (cons ~pred values#)})
           (clojure-test/do-report {:type     :fail, :message ~msg,
                                    :expected '~form,
                                    :actual   (list '~'not
                                                    (list '=* ~expected result#))}))
         result#))))

(defmethod kaocha.report/print-expr '=* [m]
  (let [printer (output/printer)]
    (let [[_ expected & actuals] (-> m :actual second)]
      (output/print-doc
        [:span
         "Expected:" :line
         [:nest (output/format-doc expected printer)]
         :break
         "Actual:" :line
         (into [:nest]
               (interpose :break)
               (for [actual actuals]
                 (output/format-doc actual
                                    printer)))]))))

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

(defn calculate-complete-costs [problem]
  (+ (:costs problem) (transduce (map heuristic) + (:stack problem))))

(defn atom-count-seq [x] (::count-seq (meta x)))
(defn atom-count [x] (reduce + 1 (atom-count-seq x)))

(defmethod heuristic 0 [comparison]
  0)

(defmethod heuristic :default [comparison]
  (atom-count (first comparison)))

(defmethod heuristic :atom [[left right]]
  (if (= left right) 0 1))

(defmethod heuristic :sequential [[left-xs right-xs :as comparison]]
  (let [[a-xs b-xs] (if (<= (count left-xs) (count right-xs))
                      comparison
                      [right-xs left-xs])
        diff-count (- (count b-xs) (count a-xs))]
    (transduce (take diff-count) + (sort (atom-count-seq b-xs)))))

(defn set|map:heuristic [[left right]]
  (if (<= (count left) (count right))
    0
    (let [diff-count (- (count left) (count right))]
      (transduce (take diff-count) + (sort (atom-count-seq left))))))

(defmethod heuristic :set [comparison]
  (set|map:heuristic comparison))

(defmethod heuristic :map [comparison]
  (set|map:heuristic comparison))

(defn add-diff [problem left]
  (-> problem
      (update :diffs conj [(:left-path problem) (:right-path problem)])
      (update :costs + (atom-count left))))

(defmethod children :default [[left] problem]
  (list (add-diff problem left)))

(defmethod children :atom [[left right] problem]
  (list (cond-> problem
                (not= left right) (add-diff left))))

(defmacro stack-updates [path-0 values-0 & [_ pred path-1 values-1]]
  `(if ~pred [[::pop] ~values-1 [::push ~path-1]
              [::pop] ~values-0 [::push ~path-0]]
             [[::pop] ~values-0 [::push ~path-0]]))

(defn seq:stack-updates-default [left-index right-index left right]
  (stack-updates [[:index left-index] [:index right-index]]
                 [left right]))

(defn seq:dis [xs] (with-meta (rest xs) (update (meta xs) ::count-seq rest)))

(defn seq:stack-updates [[_ index xs] xs-f]
  (let [xs* (xs-f xs)]
    (vary-meta xs* assoc ::index (inc index))))

(defn seq:child-default [problem
                         [left left-index :as left-indexed]
                         [right right-index :as right-indexed]]
  (-> problem
      (update :stack conj [(seq:stack-updates left-indexed seq:dis)
                           (seq:stack-updates right-indexed seq:dis)])
      (update :stack into (seq:stack-updates-default left-index right-index left right))))

(defn seq:child-delete-first-element [problem
                                      [left left-index :as left-indexed]
                                      [_ _ right-seq]]
  (-> problem
      (update :stack conj [(seq:stack-updates left-indexed seq:dis)
                           right-seq])
      (update :diffs conj [(conj (:left-path problem) [:index left-index])
                           (conj (:right-path problem) [:index -1 :nil])])
      (update :costs + (atom-count left))))

(defn seq:child-add-first-element [problem
                                   [_ left-index :as left-indexed]
                                   [right right-index :as right-indexed]]
  (-> problem
      (update :stack conj [(seq:stack-updates left-indexed identity)
                           (seq:stack-updates right-indexed seq:dis)])
      (update :diffs conj [(conj (:left-path problem) [:index left-index :before])
                           (conj (:right-path problem) [:index right-index])])
      (update :costs + (atom-count right))))

(defn seq:first-or-absent [xs]
  (if (seq xs) (first xs) ::diff/nil))

(defn seq:meta-index [xs]
  (::index (meta xs) 0))

(defmethod children :sequential [comparison problem]
  (let [[[left :as left-indexed]
         [right :as right-indexed]]
        (map (juxt seq:first-or-absent seq:meta-index identity) comparison)]
    (if (and (= left ::diff/nil) (= right ::diff/nil))
      (list problem)
      (cond-> (list)
              (and (not= left ::diff/nil) (not= right ::diff/nil))
              (conj (seq:child-default problem left-indexed right-indexed))
              (not= left ::diff/nil)
              (conj (seq:child-delete-first-element problem left-indexed right-indexed))
              (not= right ::diff/nil)
              (conj (seq:child-add-first-element problem left-indexed right-indexed))))))

(defn set|map:ensure-same-length-as [xs compare-xs nil-value]
  (cond-> xs (< (count xs) (count compare-xs)) (conj nil-value)))

(defn set|map:children
  [set|map:stack-updates dis nil-value [left right] problem]
  (if (empty? left)
    (list problem)
    (let [l (first left)
          left-1 (dis left l)]
      (for [r (-> right (set|map:ensure-same-length-as left nil-value))]
        (cond-> problem
                (seq left-1) (update :stack conj [left-1 (dis right r)])
                :always (update :stack into (set|map:stack-updates l r)))))))

(defn set:dis [s x] (vary-meta (disj s x) update ::count-seq rest))

(defn set:stack-updates [left right]
  (stack-updates [[:set left] [:set right]] [left right]))

(defmethod children :set
  [comparison problem]
  (set|map:children set:stack-updates
                    set:dis ::diff/nil comparison problem))

(defn map:dis [m [k]] (vary-meta (dissoc m k) update ::count-seq rest))

(defn map:stack-updates [[left-key left-value] [right-key right-value]]
  (stack-updates [[:m-key left-key] [:m-key right-key]]
                 [left-key right-key]
                 :when (and (not= ::diff/nil left-key)
                            (not= ::diff/nil right-key))
                 [[:m-val left-key] [:m-val right-key]]
                 [left-value right-value]))

(defmethod children :map
  [comparison problem]
  (set|map:children map:stack-updates
                    map:dis [::diff/nil ::diff/nil] comparison problem))

(defn stack? [problem]
  (boolean (seq (:stack problem))))

(defn better? [p0 p1]
  (let [stack-0? (stack? p0)
        stack-1? (stack? p1)]
    (neg? (compare [stack-0? (:costs p0)]
                   [stack-1? (:costs p1)]))))

(defn choose-better [defender challenger]
  (if (better? challenger defender)
    challenger
    defender))

(defn push-path [problem [left-path right-path]]
  (-> problem
      (update :left-path conj left-path)
      (update :right-path conj right-path)
      (update :stack pop)))

(defn pop-path [problem]
  (-> problem
      (update :left-path pop)
      (update :right-path pop)
      (update :stack pop)))

(defn update-path [problem]
  (loop [{stack :stack :as p} problem]
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

(def coll-walker+meta-nav
  (s/recursive-path
    [] p
    (s/if-path coll?
               (s/continue-then-stay
                 [s/ALL-WITH-META p]))))

(defn prepare [x]
  (s/transform coll-walker+meta-nav add-count-meta x))

(a-star/def-a-star EqualStarProblem [costs stack seen]
  a-star/CostPredictable
  (back+forward-costs [_]
    (+ costs (transduce (map heuristic) + stack)))
  search/Searchable
  (children [this]
    (a-star/with-children
      (when-let [comparison (and (not (contains? @seen stack))
                                 (peek stack))]
        (vswap! seen conj stack)
        (children comparison (update this :stack pop)))))
  (xform [_]
    (a-star/with-xform
      (map update-path)))
  search/AsyncSearchable
  (xform-async [_]
    (a-star/with-xform-async
      (map #(assoc % :seen (volatile! #{})))))
  search/ExhaustiveSearch
  (stop [{:keys [diffs] :as this}]
    (a-star/with-stop
      (when (not (stack? this))
        (cond-> this
                (empty? diffs) (reduced)))))
  search/Combinable
  (combine [_ other] other)
  (combine-async [this other] (choose-better this other)))

(defn equal-star-problem [left right]
  (let [left* (prepare left)
        right* (prepare right)]
    (-> {:source     [left* right*]
         :stack      (list [left* right*])
         :diffs      '()
         :costs      0
         :left-path  []
         :right-path []
         :seen       (volatile! #{})}
        (map->EqualStarProblem)
        (a-star/init))))

(defn =*
  ([a b] (=* a b nil))
  ([a b options]
   (as-> (equal-star-problem a b) $
         (search/parallel-depth-first-search $
                                             (merge {:parallelism 1 :chan-size 1}
                                                    options))
         (if (seq (:stack $))
           :timeout
           (diff/diff (:diffs $) (:source $))))))