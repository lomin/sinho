(ns me.lomin.sinho.pred
  "Predicate wrapper for sinho's structural matcher.

   `(pred f)` wraps a 1-arg predicate for use in expected positions.
   Bare functions in expected positions are compared by value equality,
   not interpreted as predicates. This eliminates ambiguity between
   'predicate-on-actual' and 'literal function-value equality.'")

(defrecord Pred [f label])

(defn pred
  "Wrap a 1-arg predicate for use in an expected position.
   Truthy result from (f actual) = match. Falsy = mismatch.

   Arities:
     (pred f)         - use (pr-str f) or fn metadata for failure label
     (pred f label)   - explicit label for readable failure diffs"
  ([f]
   (let [label (or (:name (meta f))
                   (when-let [n (and (var? f) (:name (meta f)))]
                     (str n))
                   nil)]
     (->Pred f label)))
  ([f label]
   (->Pred f (str label))))

(defn pred?
  "True if x is a Pred wrapper."
  [x]
  (instance? Pred x))

(defn pred-label-for-diff
  "Human-readable label for a pred in a diff context."
  [^Pred p]
  (or (:label p)
      (pr-str (:f p))))

(defn named-pred
  "Convenience: create a labelled predicate with a readable failure diff.
   Equivalent to (pred f name)."
  [name f]
  (->Pred f (str name)))
