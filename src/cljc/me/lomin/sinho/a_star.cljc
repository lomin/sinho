(ns me.lomin.sinho.a-star
  (:require [me.lomin.sinho.search :as search]))

;; Platform-agnostic max value
(def MAX_VALUE #?(:clj Integer/MAX_VALUE
                  :cljs js/Number.MAX_SAFE_INTEGER))

(defprotocol AStar
  (get-costs [self])
  (get-best-costs [self])
  (get-back+forward-costs [self])
  (seen [self])
  (forward-costs [self])
  (a-star-identity [self])
  (goal? [this]))

(defn calculate-back+forward-costs [p]
  (+ (or (get-costs p) 0) (forward-costs p)))

(defn inc-costs [p costs]
  (update p :a-star:costs + costs))

(defn depth [priority]
  (get priority 1))

(defn best-cost-xform []
  (remove #(<= (deref (get-best-costs %)) (get-back+forward-costs %))))

(defn priority-xform []
  (map #(update %
                :a-star:priority
                (fn [priority]
                  [(get-back+forward-costs %) (dec (depth priority))]))))

(defn back+forward-costs-xform []
  (map #(assoc % :a-star:back+forward-costs (calculate-back+forward-costs %))))

(defn filter-new-or-better-nodes-xform []
  (filter #(let [back+forward-costs (get-back+forward-costs %)
                 seen (seen %)
                 a-star-identity (a-star-identity %)]
             (when (< back+forward-costs (get @seen a-star-identity MAX_VALUE))
               (vswap! seen assoc a-star-identity back+forward-costs)))))

(defmacro with-xform [& body]
  (let [body* (when (seq body) (list (cons 'do body)))]
    `(comp
      (back+forward-costs-xform)
      (filter-new-or-better-nodes-xform)
      ~@body*
      (priority-xform)
      (best-cost-xform))))

(defn init
  ([root-node] (init root-node nil))
  ([root-node custom-config]
   (merge {:root-node        (merge root-node
                                    {:a-star:costs              0
                                     :a-star:seen               (volatile!
                                                                 {(a-star-identity root-node)
                                                                  (calculate-back+forward-costs root-node)})
                                     :a-star:back+forward-costs 0
                                     :a-star:priority           [0 0]
                                     :a-star:best-costs         (volatile! MAX_VALUE)})
           :compare-priority search/smaller-is-better
           :search-xf        (with-xform)}
          custom-config)))