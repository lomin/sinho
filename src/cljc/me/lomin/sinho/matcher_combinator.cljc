(ns me.lomin.sinho.matcher-combinator
  (:require
   [matcher-combinators.core :as mc]
   [matcher-combinators.result :as result]
   [matcher-combinators.model :as model]
   [me.lomin.sinho.matcher :as sinho]))

(defn calculate-weight
  "Calculate mismatch weight from sinho result using A* cost metadata.
   The A* algorithm has already computed the optimal cost during search,
   so we can reuse it directly rather than counting diff nodes."
  [sinho-result]
  (or (:sinho/cost (meta sinho-result)) 0))

(defn sinho->result
  "Converts sinho =* result to matcher-combinators result format.
   Returns a map with ::result/type, ::result/value, and ::result/weight."
  [expected sinho-result]
  (cond
    ;; Perfect match - sinho returns expected value
    (= expected sinho-result)
    {::result/type :match
     ::result/value sinho-result
     ::result/weight 0}

    ;; Timeout case
    (= :timeout sinho-result)
    {::result/type :mismatch
     ::result/value (model/->InvalidMatcherContext
                     "Sinho matcher timed out during A* search")
     ::result/weight 1}

    ;; Mismatch with diff structure
    :else
    {::result/type :mismatch
     ::result/value sinho-result ; Already contains diff structure
     ::result/weight (calculate-weight sinho-result)}))

(defrecord EqualStar [expected]
  mc/Matcher
  (-matcher-for [this] this)
  (-matcher-for [this _] this)
  (-match [_this actual]
    (let [sinho-result (sinho/=* expected actual)]
      (sinho->result expected sinho-result)))
  (-base-name [_] 'sinho-equals))

(defn =*
  "Creates a strict equality matcher using sinho's =* function."
  [expected]
  (->EqualStar expected))