(ns me.lomin.sinho.matcher
  "Coinductive IR decision engine for structural subset matching.

   v3 replaces sinho 2.2's A* search with a direct structural walk:
   - Maps: O(n) via key lookup (no pairings search)
   - Sets: O(n^3) via Kuhn's augmenting-paths for verdicts; greedy O(n^2 log n) for diffs
   - Sequences: O(m*n) via tree-edit-distance DP
   - Atoms: O(1) direct equality
   - Predicates: (pred f) wrapper dispatch

   The coinductive assumption set (seen pairs) prevents infinite loops
   on recursive structures, analogous to sinho 2.2's seen map."
  (:require
   #?(:bb [clojure.test :as test]
      :clj [clojure.test :as test]
      :cljs [cljs.test :as test])
   [clojure.walk :as walk]
   [com.rpl.specter :as s]
   [matcher-combinators.model :refer
    [->Missing ->Unexpected ->Mismatch]]
   #?@(:bb []
       :clj [[lambdaisland.deep-diff2.diff-impl :as diff2]
             [kaocha.report :as report]]
       :cljs [[lambdaisland.deep-diff2.diff-impl :as diff2]])
   [me.lomin.sinho.diff :as diff]
   [me.lomin.sinho.pred :as pred]
   [me.lomin.sinho.timeout :as timeout])
  #?(:cljs (:require-macros [me.lomin.sinho.matcher-test-macros])))

;; ── Test assertion ──────────────────────────────────────────────

#?(:clj
   (defmethod test/assert-expr '=*
     [msg form]
     (let [[_ expected] form]
       `(let [result# ~form]
          (test/do-report {:type (if (= ~expected result#) :pass :fail)
                           :message ~msg
                           :expected '~form
                           :actual result#})
          result#))))

;; ── Diff-type helpers ───────────────────────────────────────────

(def unexpected-type (type (->Unexpected nil)))
(def missing-type (type (->Missing nil)))
(def mismatch-type (type (->Mismatch nil nil)))

(defn insertion? [x]
  (= (type x) unexpected-type))

(defn deletion? [x]
  (= (type x) missing-type))

(defn mismatch? [x]
  (= (type x) mismatch-type))

;; ── deep-diff2 / kaocha integration ────────────────────────────

#?(:bb nil
   :default
   (defn to-diff2
     [form]
     (walk/postwalk (fn [x]
                      (cond (insertion? x) (diff2/->Insertion (:actual x))
                            (deletion? x) (diff2/->Deletion (:expected x))
                            (mismatch? x) (diff2/->Mismatch (:expected x)
                                                            (:actual x))
                            :else x))
                    form)))

#?(:bb nil
   :clj
   (defmethod kaocha.report/print-expr '=* [m]
     (if (= (:actual m) :timeout)
       (report/print-expression (update m :actual list))
       (report/print-expression (update m :actual to-diff2)))))

;; ── Type dispatch ───────────────────────────────────────────────

(defn equality-partition
  "Classify a pair for structural dispatch."
  [expected actual]
  (cond
    (pred/pred? expected)   :pred
    (= expected actual)     :equal
    (sequential? expected)  (if (sequential? actual) :sequential :default)
    (map? expected)         (if (map? actual) :map :default)
    (set? expected)         (if (set? actual) :set :default)
    :else                   :atom))

;; ── Coinductive engine state ────────────────────────────────────

(defn- make-ctx
  "Create a matching context with coinductive assumption set."
  []
  {:seen #{}})

(defn- assume-pair
  "Record a pair as assumed-equal in the coinductive set."
  [ctx expected actual]
  (update ctx :seen conj [expected actual]))

(defn- pair-assumed?
  "Check if a pair is already in the assumption set."
  [ctx expected actual]
  (contains? (:seen ctx) [expected actual]))

;; ── Maximum bipartite matching (augmenting paths) ───────────────
;; Used for verdict-correct set/map matching: find whether a perfect
;; matching exists on the zero-cost subgraph before falling back to
;; greedy for diff generation.

(defn- augmenting-path?
  "Try to find an augmenting path from left node u.
   adj-fn: (fn [i]) -> set of right indices that i can match to (cost 0).
   match-r: volatile map of right-index -> left-index assignments.
   visited: volatile set of visited right indices in this DFS."
  [u adj-fn match-r visited]
  (some (fn [v]
          (when-not (contains? @visited v)
            (vswap! visited conj v)
            (let [prev (get @match-r v)]
              (when (or (nil? prev)
                        (augmenting-path? prev adj-fn match-r visited))
                (vswap! match-r assoc v u)
                true))))
        (adj-fn u)))

(defn- max-bipartite-matching-count
  "Find maximum bipartite matching size using augmenting paths.
   adj-fn: (fn [left-idx]) -> seq of right indices with cost 0.
   m: number of left nodes."
  [adj-fn m]
  (let [match-r (volatile! {})]
    (reduce (fn [count i]
              (let [visited (volatile! #{})]
                (if (augmenting-path? i adj-fn match-r visited)
                  (inc count)
                  count)))
            0
            (range m))))

(defn- max-bipartite-matching
  "Find maximum bipartite matching, return the assignment map.
   adj-fn: (fn [left-idx]) -> seq of right indices with cost 0.
   m: number of left nodes.
   Returns: map of left-idx -> right-idx for matched pairs."
  [adj-fn m]
  (let [match-r (volatile! {})]
    (doseq [i (range m)]
      (let [visited (volatile! #{})]
        (augmenting-path? i adj-fn match-r visited)))
    ;; Invert: match-r is right->left, we want left->right
    (reduce-kv (fn [acc r l] (assoc acc l r)) {} @match-r)))

;; ── Cost computation (for set matching) ─────────────────────────

(declare match-cost)

(defn- atom-cost
  "Cost of matching two atoms: 0 if equal, 1 if not."
  [expected actual]
  (if (= expected actual) 0 1))

(defn- sequential-cost
  "Cost of matching two sequences via edit-distance DP.
   Subset semantics: extra elements in actual (insertions) cost 0.
   Missing elements from expected (deletions) cost 1 each."
  [ctx expected actual]
  (let [m (count expected)
        n (count actual)
        exp-vec (vec expected)
        act-vec (vec actual)
        idx (fn [i j] (+ (* i (inc n)) j))
        dp (long-array (* (inc m) (inc n)))]
    ;; Base: deleting all expected elements costs 1 each
    (dotimes [i (inc m)]
      (aset dp (idx i 0) (long i)))
    ;; Base: inserting all actual elements costs 0 (subset semantics)
    (dotimes [j (inc n)]
      (aset dp (idx 0 j) 0))
    ;; Fill DP table
    (dotimes [i m]
      (dotimes [j n]
        (let [sub-cost (match-cost ctx (nth exp-vec i) (nth act-vec j))
              replace-cost (+ (aget dp (idx i j)) sub-cost)
              delete-cost  (+ (aget dp (idx i (inc j))) 1)
              insert-cost  (+ (aget dp (idx (inc i) j)) 0)] ; insertion = 0 cost
          (aset dp (idx (inc i) (inc j))
                (long (min replace-cost delete-cost insert-cost))))))
    (aget dp (idx m n))))

(defn- build-cost-matrix
  "Build adjacency data for set/map bipartite matching.
   Returns {:zero-adj (fn [i] -> seq of j with cost 0)}."
  [cost-fn m n]
  (let [pairs (for [i (range m)
                    j (range n)]
                {:i i :j j :cost (cost-fn i j)})
        zero-adj (reduce (fn [acc {:keys [i j cost]}]
                           (if (zero? cost)
                             (update acc i (fnil conj []) j)
                             acc))
                         {}
                         pairs)]
    {:zero-adj (fn [i] (get zero-adj i []))}))

(defn- greedy-assignment
  "Greedy bipartite assignment from sorted pairs."
  [sorted-pairs]
  (reduce (fn [{:keys [used-i used-j total pairs] :as acc}
               {:keys [i j cost]}]
            (if (or (contains? used-i i) (contains? used-j j))
              acc
              {:used-i (conj used-i i)
               :used-j (conj used-j j)
               :total (+ total cost)
               :pairs (conj pairs [i j])}))
          {:used-i #{} :used-j #{} :total 0 :pairs []}
          sorted-pairs))

(defn- map-entry-cost
  "Cost of matching two map entries (key-pair + value-pair)."
  [ctx [ek ev] [ak av]]
  (+ (match-cost ctx ek ak) (match-cost ctx ev av)))

(defn- map-cost
  "Cost of matching two maps.
   First tries exact key lookup (O(1) per key). For left keys not found
   exactly in right, uses exact bipartite matching on zero-cost subgraph
   for verdict correctness, then greedy for cost estimation."
  [ctx expected actual]
  (let [;; Phase 1: exact key matches
        exact-keys (filter #(contains? actual %) (keys expected))
        exact-cost (reduce (fn [c k]
                             (+ c (match-cost ctx (get expected k) (get actual k))))
                           0 exact-keys)
        ;; Phase 2: remaining left entries need bipartite matching
        remaining-left (apply dissoc expected exact-keys)
        remaining-right (apply dissoc actual exact-keys)]
    (if (empty? remaining-left)
      exact-cost
      (let [l-entries (vec remaining-left)
            r-entries (vec remaining-right)
            m (count l-entries)
            n (count r-entries)
            cost-fn (fn [i j] (map-entry-cost ctx
                                               (nth l-entries i)
                                               (nth r-entries j)))
            {:keys [zero-adj]} (build-cost-matrix cost-fn m n)
            match-count (max-bipartite-matching-count zero-adj m)]
        (if (= match-count m)
          exact-cost  ; All remaining entries matched at zero cost
          ;; Greedy for cost estimation
          (let [pairs (for [i (range m) j (range n)]
                        {:i i :j j
                         :cost (cost-fn i j)
                         :hash (hash (str (pr-str (nth l-entries i))
                                          (pr-str (nth r-entries j))))})
                sorted (sort-by (juxt :cost :hash) pairs)
                {:keys [total used-i]} (greedy-assignment sorted)
                unmatched (- m (count used-i))]
            (+ exact-cost total unmatched)))))))

(defn- set-cost-for-pair
  "Cost of matching an individual set element pair."
  [ctx l r]
  (match-cost ctx l r))

(defn- set-cost
  "Cost of matching two sets. Uses exact maximum bipartite matching
   on the zero-cost subgraph for correct verdicts, then greedy for cost."
  [ctx expected actual]
  (if (every? (fn [e] (contains? actual e)) expected)
    0
    (let [exp-vec (vec expected)
          act-vec (vec actual)
          m (count exp-vec)
          n (count act-vec)
          cost-fn (fn [i j] (set-cost-for-pair ctx
                                                (nth exp-vec i)
                                                (nth act-vec j)))
          {:keys [zero-adj]} (build-cost-matrix cost-fn m n)
          ;; Exact matching: can all left elements be matched at cost 0?
          match-count (max-bipartite-matching-count zero-adj m)]
      (if (= match-count m)
        0  ; All left elements have zero-cost matches
        ;; Fall back to greedy for cost estimation
        (let [pairs (for [i (range m) j (range n)]
                      {:i i :j j
                       :cost (cost-fn i j)
                       :hash (hash (str (pr-str (nth exp-vec i))
                                        (pr-str (nth act-vec j))))})
              sorted (sort-by (juxt :cost :hash) pairs)
              {:keys [total used-i]} (greedy-assignment sorted)
              unmatched (- m (count used-i))]
          (+ total unmatched))))))

(defn- pred-cost
  "Cost of a pred match: 0 if predicate passes, 1 otherwise."
  [expected actual]
  (if ((:f expected) actual) 0 1))

(defn match-cost
  "Compute the match cost between expected and actual.
   Uses coinductive assumption set to handle recursive structures."
  [ctx expected actual]
  (if (pair-assumed? ctx expected actual)
    0
    (let [ctx' (assume-pair ctx expected actual)]
      (case (equality-partition expected actual)
        :equal      0
        :pred       (pred-cost expected actual)
        :atom       (atom-cost expected actual)
        :sequential (sequential-cost ctx' expected actual)
        :map        (map-cost ctx' expected actual)
        :set        (set-cost ctx' expected actual)
        :default    1))))

;; ── Diff generation ─────────────────────────────────────────────
;; The engine walks expected, dispatches on type, generates diff paths
;; compatible with sinho's existing diff.cljc path-tree infrastructure.

(declare coinductive-diff)

(defn- diff-atom
  "Diff two atoms. Returns a list of diff-path pairs."
  [_ctx left-path right-path expected actual]
  (if (= expected actual)
    '()
    (list [left-path right-path])))

(defn- diff-pred
  "Diff a pred-wrapped expected against actual."
  [_ctx left-path right-path expected actual]
  (if ((:f expected) actual)
    '()
    (list [left-path right-path])))

(defn- diff-default
  "Diff when types are incompatible."
  [_ctx left-path right-path _expected _actual]
  (list [left-path right-path]))

(defn- diff-map-entry
  "Diff a matched map entry pair: diff key then value."
  [ctx left-path right-path [ek ev] [ak av]]
  (let [key-diffs (coinductive-diff ctx
                                     (conj left-path [:m-key ek])
                                     (conj right-path [:m-key ak])
                                     ek ak)
        val-diffs (coinductive-diff ctx
                                     (conj left-path [:m-val ek])
                                     (conj right-path [:m-val ak])
                                     ev av)]
    (concat key-diffs val-diffs)))

(defn- diff-map
  "Diff two maps. Exact key lookup first, then exact bipartite matching
   on zero-cost subgraph for verdict correctness, with greedy fallback
   for diff generation."
  [ctx left-path right-path expected actual]
  (let [;; Phase 1: exact key matches
        exact-keys (filter #(contains? actual %) (keys expected))
        exact-diffs (mapcat
                     (fn [k]
                       (coinductive-diff ctx
                                         (conj left-path [:m-val k])
                                         (conj right-path [:m-val k])
                                         (get expected k)
                                         (get actual k)))
                     exact-keys)
        ;; Phase 2: remaining entries
        remaining-left (apply dissoc expected exact-keys)
        remaining-right (apply dissoc actual exact-keys)]
    (if (empty? remaining-left)
      exact-diffs
      ;; Bipartite matching on remaining map entries
      (let [l-entries (vec remaining-left)
            r-entries (vec remaining-right)
            m (count l-entries)
            n (count r-entries)
            cost-fn (fn [i j] (map-entry-cost (make-ctx)
                                               (nth l-entries i)
                                               (nth r-entries j)))
            {:keys [zero-adj]} (build-cost-matrix cost-fn m n)
            exact-match (max-bipartite-matching zero-adj m)]
        (if (= (count exact-match) m)
          ;; All remaining entries matched at zero cost: recurse for sub-diffs
          (let [matched-diffs (mapcat
                               (fn [[li ri]]
                                 (diff-map-entry ctx left-path right-path
                                                 (nth l-entries li)
                                                 (nth r-entries ri)))
                               exact-match)]
            (concat exact-diffs matched-diffs))
          ;; Greedy fallback for diff generation
          (let [pairs (for [i (range m) j (range n)]
                        {:i i :j j
                         :cost (cost-fn i j)
                         :hash (hash (str (pr-str (nth l-entries i))
                                          (pr-str (nth r-entries j))))})
                sorted (sort-by (juxt :cost :hash) pairs)
                {:keys [used-i pairs]} (greedy-assignment sorted)
                matched-diffs (mapcat
                               (fn [[i j]]
                                 (diff-map-entry ctx left-path right-path
                                                 (nth l-entries i)
                                                 (nth r-entries j)))
                               pairs)
                unmatched-left (remove used-i (range m))
                missing-diffs (map (fn [i]
                                     (let [[k _v] (nth l-entries i)]
                                       [(conj left-path [:m-key k])
                                        (conj right-path [:m-key ::diff/nil])]))
                                   unmatched-left)]
            (concat exact-diffs matched-diffs missing-diffs)))))))

;; ── Sequential diff via edit-distance DP with backtracking ──────

(defn- build-seq-dp
  "Build DP table for sequential edit distance with subset semantics."
  [ctx exp-vec act-vec m n]
  (let [idx (fn [i j] (+ (* i (inc n)) j))
        dp (long-array (* (inc m) (inc n)))]
    (dotimes [i (inc m)]
      (aset dp (idx i 0) (long i)))
    (dotimes [j (inc n)]
      (aset dp (idx 0 j) 0))
    (dotimes [i m]
      (dotimes [j n]
        (let [sub-cost (match-cost (make-ctx)
                                   (nth exp-vec i) (nth act-vec j))
              replace-cost (+ (aget dp (idx i j)) sub-cost)
              delete-cost  (+ (aget dp (idx i (inc j))) 1)
              insert-cost  (+ (aget dp (idx (inc i) j)) 0)]
          (aset dp (idx (inc i) (inc j))
                (long (min replace-cost delete-cost insert-cost))))))
    dp))

(defn- backtrack-seq-edits
  "Backtrack through DP table to produce a forward edit script.
   Returns a sequence of [:match i j], [:delete i], [:insert j] ops."
  [dp exp-vec act-vec m n]
  (let [idx (fn [i j] (+ (* i (inc n)) j))]
    (loop [i m, j n, ops '()]
      (cond
        (and (zero? i) (zero? j))
        ops

        (and (pos? i) (pos? j)
             (let [sub-cost (match-cost (make-ctx)
                                        (nth exp-vec (dec i))
                                        (nth act-vec (dec j)))]
               (= (aget dp (idx i j))
                  (+ (aget dp (idx (dec i) (dec j))) sub-cost))))
        (recur (dec i) (dec j) (cons [:match (dec i) (dec j)] ops))

        (and (pos? i)
             (= (aget dp (idx i j))
                (+ (aget dp (idx (dec i) j)) 1)))
        (recur (dec i) j (cons [:delete (dec i)] ops))

        :else
        (recur i (dec j) (cons [:insert (dec j)] ops))))))

(defn- seq-edits->diff-paths
  "Convert forward edit script to diff paths, tracking indices.
   Insertions get unique left-paths via a virtual output index
   that advances for all operations (matches, deletes, inserts)."
  [ctx left-path right-path exp-vec act-vec ops]
  (loop [ops ops
         out-idx 0  ; virtual output index, advances for every op
         diffs '()]
    (if (empty? ops)
      diffs
      (let [[op & args] (first ops)]
        (case op
          :match
          (let [[ei ai] args
                lp (conj left-path [:index out-idx])
                rp (conj right-path [:index ai])
                sub-diffs (coinductive-diff ctx lp rp
                                            (nth exp-vec ei)
                                            (nth act-vec ai))]
            (recur (rest ops) (inc out-idx) (into diffs sub-diffs)))

          :delete
          (let [[_ei] args
                lp (conj left-path [:index out-idx])
                rp (conj right-path [:index -1 :nil])]
            (recur (rest ops) (inc out-idx) (conj diffs [lp rp])))

          :insert
          (let [[ai] args
                lp (conj left-path [:index out-idx :before])
                rp (conj right-path [:index ai])]
            (recur (rest ops) (inc out-idx) (conj diffs [lp rp]))))))))

(defn- diff-sequential
  "Diff two sequences using edit-distance DP with forward edit script.
   Subset semantics: extra elements in actual (insertions) cost 0."
  [ctx left-path right-path expected actual]
  (let [exp-vec (vec expected)
        act-vec (vec actual)
        m (count exp-vec)
        n (count act-vec)]
    (if (and (zero? m) (zero? n))
      '()
      (let [dp (build-seq-dp ctx exp-vec act-vec m n)
            ops (backtrack-seq-edits dp exp-vec act-vec m n)]
        (seq-edits->diff-paths ctx left-path right-path
                               exp-vec act-vec ops)))))

;; ── Set diff via greedy bipartite matching ──────────────────────

(defn- stable-hash-for-pair
  "Deterministic hash for a (left, right) pair for tie-breaking."
  [l r]
  (hash (str (pr-str l) (pr-str r))))

(defn- diff-set
  "Diff two sets. Uses exact bipartite matching on zero-cost subgraph
   for correct verdicts, then greedy for diff path generation."
  [ctx left-path right-path expected actual]
  (if (every? (fn [e] (contains? actual e)) expected)
    '()
    (let [exp-vec (vec expected)
          act-vec (vec actual)
          m (count exp-vec)
          n (count act-vec)
          cost-fn (fn [i j] (match-cost (make-ctx)
                                         (nth exp-vec i)
                                         (nth act-vec j)))
          {:keys [zero-adj]} (build-cost-matrix cost-fn m n)
          ;; Exact matching: can all left elements be matched at cost 0?
          exact-match (max-bipartite-matching zero-adj m)]
      (if (= (count exact-match) m)
        ;; All matched at zero cost: no diffs needed
        '()
        ;; Some elements unmatched: use greedy for diff generation
        (let [pairs (for [i (range m) j (range n)]
                      {:i i :j j
                       :cost (cost-fn i j)
                       :hash (stable-hash-for-pair (nth exp-vec i)
                                                   (nth act-vec j))})
              sorted (sort-by (juxt :cost :hash) pairs)
              {:keys [used-i pairs]} (greedy-assignment sorted)
              matched-diffs (mapcat
                             (fn [[i j]]
                               (let [l (nth exp-vec i)
                                     r (nth act-vec j)
                                     lp (conj left-path [:set l])
                                     rp (conj right-path [:set r])]
                                 (coinductive-diff ctx lp rp l r)))
                             pairs)
              unmatched-left (remove used-i (range m))
              missing-diffs (map (fn [i]
                                   (let [l (nth exp-vec i)
                                         lp (conj left-path [:set l])
                                         rp (conj right-path [:set ::diff/nil])]
                                     [lp rp]))
                                 unmatched-left)]
          (concat matched-diffs missing-diffs))))))

;; ── Main coinductive dispatch ───────────────────────────────────

(defn coinductive-diff
  "Walk expected, dispatch on type, generate diff paths.
   Uses coinductive assumption set to handle recursive structures."
  [ctx left-path right-path expected actual]
  (if (pair-assumed? ctx expected actual)
    '()
    (let [ctx' (assume-pair ctx expected actual)]
      (case (equality-partition expected actual)
        :equal      '()
        :pred       (diff-pred ctx' left-path right-path expected actual)
        :atom       (diff-atom ctx' left-path right-path expected actual)
        :sequential (diff-sequential ctx' left-path right-path expected actual)
        :map        (diff-map ctx' left-path right-path expected actual)
        :set        (diff-set ctx' left-path right-path expected actual)
        :default    (diff-default ctx' left-path right-path expected actual)))))

;; ── Prepare (metadata for atom-count, preserved from v2.2) ─────

(defn atom-count-seq [x] (::count-seq (meta x)))
(defn atom-count [x] (reduce + 1 (atom-count-seq x)))

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

;; ── Public API ──────────────────────────────────────────────────

(defn compute-diff-paths
  "Run the coinductive engine to produce diff paths."
  [left right]
  (coinductive-diff (make-ctx) [] [] left right))

(defn compute-cost
  "Compute the total match cost."
  [left right]
  (match-cost (make-ctx) left right))

(defn =*
  "Structural subset matcher: is expected a structural subset of actual?
   Returns expected on match, a diff structure on mismatch.
   Options map supports :timeout (ms) for cooperative timeout."
  ([a b] (=* a b nil))
  ([a b options]
   (let [timeout-ms (:timeout options 1000)
         timeout-fn (when timeout-ms (timeout/make-timeout timeout-ms))
         left (prepare a)
         right (prepare b)]
     ;; Check timeout before starting diff computation
     (if (and timeout-fn (timeout-fn))
       :timeout
       (let [diff-paths (compute-diff-paths left right)]
         (if (empty? diff-paths)
           ;; Perfect match: return expected
           a
           ;; Mismatch: produce diff via the path-tree infrastructure
           (let [diff-result (diff/diff diff-paths [left right])
                 cost (compute-cost left right)]
             (if #?(:cljs (satisfies? IWithMeta diff-result)
                    :default (instance? clojure.lang.IObj diff-result))
               (with-meta diff-result {:sinho/cost cost})
               diff-result))))))))
