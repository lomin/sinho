(ns me.lomin.sinho.bipartite
  "Bipartite-matching primitives for structural subset reasoning.

   Implements Kuhn's augmenting-paths algorithm — a graph traversal that
   finds the maximum number of schema clauses that can be paired without
   reuse. Used by me.lomin.sinho.matcher to check structural exhaustiveness
   in set and map matching.

   This is purely graph-algorithmic and is not coinductive in the
   type-theory sense.")

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

(defn max-bipartite-matching-count
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

(defn max-bipartite-matching
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

;; ── Cost matrix and greedy assignment ───────────────────────────

(defn build-cost-matrix
  "Build adjacency data for bipartite matching.
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

(defn greedy-assignment
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
