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
   [me.lomin.sinho.bipartite :as bipartite]
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

;; --- coinductive ---

(defn make-ctx
  "Create a matching context with coinductive assumption set."
  []
  {:seen #{}})

(defn assume-pair
  "Record a pair as assumed-equal in the coinductive set."
  [ctx expected actual]
  (update ctx :seen conj [expected actual]))

(defn pair-assumed?
  "Check if a pair is already in the assumption set."
  [ctx expected actual]
  (contains? (:seen ctx) [expected actual]))

;; ── Cost computation (for set matching) ─────────────────────────

(declare match-cost)

(defn- stable-hash-for-pair
  "Deterministic hash for a (left, right) pair for tie-breaking."
  [l r]
  (hash [l r]))

(defn- sorted-cost-pairs
  "All (i,j) pairs ordered by (cost, stable-hash) for greedy assignment."
  [cost-fn l-vec r-vec m n]
  (sort-by (juxt :cost :hash)
           (for [i (range m) j (range n)]
             {:i i :j j
              :cost (cost-fn i j)
              :hash (stable-hash-for-pair (nth l-vec i) (nth r-vec j))})))

(defn- match-or-greedy-cost
  "Verdict-correct cost for a bipartite assignment problem.
   Returns 0 if every left node matches at cost 0; otherwise greedy total
   plus 1 per unmatched left node."
  [cost-fn l-vec r-vec m n]
  (let [{:keys [zero-adj]} (bipartite/build-cost-matrix cost-fn m n)]
    (if (= (bipartite/max-bipartite-matching-count zero-adj m) m)
      0
      (let [{:keys [total used-i]}
            (bipartite/greedy-assignment
             (sorted-cost-pairs cost-fn l-vec r-vec m n))]
        (+ total (- m (count used-i)))))))

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
  (let [exact-keys (filter #(contains? actual %) (keys expected))
        exact-cost (reduce (fn [c k]
                             (+ c (match-cost ctx (get expected k) (get actual k))))
                           0 exact-keys)
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
                                              (nth r-entries j)))]
        (+ exact-cost
           (match-or-greedy-cost cost-fn l-entries r-entries m n))))))

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
          cost-fn (fn [i j] (match-cost ctx
                                        (nth exp-vec i)
                                        (nth act-vec j)))]
      (match-or-greedy-cost cost-fn exp-vec act-vec m n))))

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
;; `coinductive-diff` walks expected, dispatches on type, and emits diff
;; paths compatible with sinho's existing diff.cljc path-tree infrastructure.

(declare coinductive-diff)

(defn- bipartite-zero-match
  "Bipartite matching on the zero-cost subgraph. Returns left→right map."
  [cost-fn m n]
  (let [{:keys [zero-adj]} (bipartite/build-cost-matrix cost-fn m n)]
    (bipartite/max-bipartite-matching zero-adj m)))

(defn- combine-results
  "Aggregate a sequence of {:paths :cost} maps into one."
  [results]
  {:paths (mapcat :paths results)
   :cost (reduce + 0 (map :cost results))})

(defn- greedy-pair-diffs
  "Greedy assignment over (l-vec, r-vec) with tie-broken ordering.
   `matched-fn [l r]` and `missing-fn [l]` each return {:paths :cost}."
  [cost-fn l-vec r-vec m n matched-fn missing-fn]
  (let [{:keys [used-i pairs]}
        (bipartite/greedy-assignment (sorted-cost-pairs cost-fn l-vec r-vec m n))
        matched-results (map (fn [[i j]] (matched-fn (nth l-vec i) (nth r-vec j)))
                             pairs)
        missing-results (map (fn [i] (missing-fn (nth l-vec i)))
                             (remove used-i (range m)))]
    (combine-results (concat matched-results missing-results))))

(defn- match-or-greedy-diff
  "Verdict-correct diff for a bipartite assignment problem.
   On exact zero-cost match, applies `matched-handler` to the left→right
   assignment. Otherwise falls back to greedy with `matched-fn`/`missing-fn`."
  [cost-fn l-vec r-vec m n matched-handler matched-fn missing-fn]
  (let [exact-match (bipartite-zero-match cost-fn m n)]
    (if (= (count exact-match) m)
      (matched-handler exact-match)
      (greedy-pair-diffs cost-fn l-vec r-vec m n matched-fn missing-fn))))

(defn- diff-atom
  [_ctx left-path right-path expected actual]
  (if (= expected actual)
    {:paths '() :cost 0}
    {:paths (list [left-path right-path]) :cost 1}))

(defn- diff-pred
  [_ctx left-path right-path expected actual]
  (if ((:f expected) actual)
    {:paths '() :cost 0}
    {:paths (list [left-path right-path]) :cost 1}))

(defn- diff-default
  [_ctx left-path right-path _expected _actual]
  {:paths (list [left-path right-path]) :cost 1})

(defn- diff-map-entry
  "Diff a matched map entry pair: diff key then value."
  [ctx left-path right-path [ek ev] [ak av]]
  (combine-results
   [(coinductive-diff ctx
                      (conj left-path [:m-key ek])
                      (conj right-path [:m-key ak])
                      ek ak)
    (coinductive-diff ctx
                      (conj left-path [:m-val ek])
                      (conj right-path [:m-val ak])
                      ev av)]))

(defn- diff-map-exact-key-diffs
  "Phase 1: recurse into values of keys present in both maps."
  [ctx left-path right-path expected actual exact-keys]
  (combine-results
   (map (fn [k]
          (coinductive-diff ctx
                            (conj left-path [:m-val k])
                            (conj right-path [:m-val k])
                            (get expected k) (get actual k)))
        exact-keys)))

(defn- diff-map
  "Diff two maps. Exact key lookup first, then bipartite matching on the
   zero-cost subgraph for verdict correctness, with greedy fallback for
   diff generation on the remaining entries."
  [ctx left-path right-path expected actual]
  (let [exact-keys (filter #(contains? actual %) (keys expected))
        exact-result (diff-map-exact-key-diffs ctx left-path right-path
                                               expected actual exact-keys)
        l-entries (vec (apply dissoc expected exact-keys))
        r-entries (vec (apply dissoc actual exact-keys))
        m (count l-entries)
        n (count r-entries)]
    (if (zero? m)
      exact-result
      (let [cost-fn (fn [i j] (map-entry-cost ctx
                                              (nth l-entries i)
                                              (nth r-entries j)))
            matched-fn (fn [l-entry r-entry]
                         (diff-map-entry ctx left-path right-path l-entry r-entry))
            missing-fn (fn [[k _v]]
                         {:paths [[(conj left-path [:m-key k])
                                   (conj right-path [:m-key ::diff/nil])]]
                          :cost 1})
            phase2 (match-or-greedy-diff
                    cost-fn l-entries r-entries m n
                    (fn [exact-match]
                      (combine-results
                       (map (fn [[li ri]]
                              (matched-fn (nth l-entries li) (nth r-entries ri)))
                            exact-match)))
                    matched-fn missing-fn)]
        (combine-results [exact-result phase2])))))

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
        (let [sub-cost (match-cost ctx (nth exp-vec i) (nth act-vec j))
              replace-cost (+ (aget dp (idx i j)) sub-cost)
              delete-cost  (+ (aget dp (idx i (inc j))) 1)
              insert-cost  (+ (aget dp (idx (inc i) j)) 0)]
          (aset dp (idx (inc i) (inc j))
                (long (min replace-cost delete-cost insert-cost))))))
    dp))

(defn- backtrack-seq-edits
  "Backtrack through DP table to produce a forward edit script.
   Returns a sequence of [:match i j], [:delete i], [:insert j] ops."
  [ctx dp exp-vec act-vec m n]
  (let [idx (fn [i j] (+ (* i (inc n)) j))]
    (loop [i m, j n, ops '()]
      (cond
        (and (zero? i) (zero? j))
        ops

        (and (pos? i) (pos? j)
             (let [sub-cost (match-cost ctx
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

(defn- seq-op->result
  "Convert one forward-edit op into a {:paths :cost} result."
  [ctx left-path right-path exp-vec act-vec out-idx [op & args]]
  (case op
    :match
    (let [[ei ai] args]
      (coinductive-diff ctx
                        (conj left-path [:index out-idx])
                        (conj right-path [:index ai])
                        (nth exp-vec ei) (nth act-vec ai)))

    :delete
    {:paths [[(conj left-path [:index out-idx])
              (conj right-path [:index -1 :nil])]]
     :cost 1}

    :insert
    (let [[ai] args]
      {:paths [[(conj left-path [:index out-idx :before])
                (conj right-path [:index ai])]]
       :cost 0})))

(defn- seq-edits->diff-paths
  "Fold a forward edit script into a combined {:paths :cost} result."
  [ctx left-path right-path exp-vec act-vec ops]
  (loop [ops ops, out-idx 0, paths [], cost 0]
    (if (empty? ops)
      {:paths paths :cost cost}
      (let [r (seq-op->result ctx left-path right-path
                              exp-vec act-vec out-idx (first ops))]
        (recur (rest ops)
               (inc out-idx)
               (into paths (:paths r))
               (+ cost (:cost r)))))))

(defn- diff-sequential
  "Diff two sequences using edit-distance DP with forward edit script.
   Subset semantics: extra elements in actual (insertions) cost 0."
  [ctx left-path right-path expected actual]
  (let [exp-vec (vec expected)
        act-vec (vec actual)
        m (count exp-vec)
        n (count act-vec)]
    (if (and (zero? m) (zero? n))
      {:paths '() :cost 0}
      (let [dp (build-seq-dp ctx exp-vec act-vec m n)
            ops (backtrack-seq-edits ctx dp exp-vec act-vec m n)]
        (seq-edits->diff-paths ctx left-path right-path
                               exp-vec act-vec ops)))))

;; ── Set diff via greedy bipartite matching ──────────────────────

(defn- diff-set
  "Diff two sets. Exact bipartite matching on the zero-cost subgraph for
   verdict correctness; greedy fallback for diff path generation."
  [ctx left-path right-path expected actual]
  (if (every? #(contains? actual %) expected)
    {:paths '() :cost 0}
    (let [exp-vec (vec expected)
          act-vec (vec actual)
          m (count exp-vec)
          n (count act-vec)
          cost-fn (fn [i j] (match-cost ctx (nth exp-vec i) (nth act-vec j)))]
      (match-or-greedy-diff
       cost-fn exp-vec act-vec m n
       (constantly {:paths '() :cost 0})
       (fn [l r]
         (coinductive-diff ctx
                           (conj left-path [:set l])
                           (conj right-path [:set r])
                           l r))
       (fn [l]
         {:paths [[(conj left-path [:set l])
                   (conj right-path [:set ::diff/nil])]]
          :cost 1})))))

;; ── Main coinductive dispatch ───────────────────────────────────

(defn coinductive-diff
  "Walk expected, dispatch on type, and return {:paths :cost}.
   Uses coinductive assumption set to handle recursive structures."
  [ctx left-path right-path expected actual]
  (if (pair-assumed? ctx expected actual)
    {:paths '() :cost 0}
    (let [ctx' (assume-pair ctx expected actual)]
      (case (equality-partition expected actual)
        :equal      {:paths '() :cost 0}
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
  (:paths (coinductive-diff (make-ctx) [] [] left right)))

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
     (if (and timeout-fn (timeout-fn))
       :timeout
       (let [{:keys [paths cost]} (coinductive-diff (make-ctx) [] [] left right)]
         (if (empty? paths)
           a
           (let [diff-result (diff/diff paths [left right])]
             (if #?(:cljs (satisfies? IWithMeta diff-result)
                    :default (instance? clojure.lang.IObj diff-result))
               (with-meta diff-result {:sinho/cost cost})
               diff-result))))))))
