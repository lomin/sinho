(ns me.lomin.sinho.search
  ^{:doc "chatda (kor. 찾다, to search) is a library to search for stuff in parallel"}
  (:require [me.lomin.sinho.timeout :as timeout])
  #?(:clj
     (:import (java.util.concurrent TimeoutException)
              (clojure.lang IPersistentStack Counted IEditableCollection
                            ITransientCollection IMeta IObj)
              (java.util PriorityQueue Comparator)))
  #?(:cljs
     (:require [tailrecursion.priority-map :as pm])))

#?(:clj (set! *warn-on-reflection* true))

(def IDENTITY-XFORM (map identity))

#?(:cljs
   (deftype TimeoutException [message]
     Object
     (toString [_] message)))

;; Pre-create exception for performance
(def timeout-exception #?(:clj (TimeoutException. "Timeout")
                          :cljs (TimeoutException. "Timeout")))

;; # Protocols
(defprotocol SearchableNode
  (children [self])
  (priority [self])
  (stop [this children])
  (combine [this previous]))

;; # Comparators
(defn priority-comparator [compare-priority]
  (fn [a b] (compare-priority (priority a) (priority b))))

(def smaller-is-better compare)
(def larger-is-better (fn [a b] (compare b a)))

;; # Platform-specific Heap Implementation

#?(:clj
   ;; CLJ: High-performance custom heap using Java's PriorityQueue
   (do
     (declare make-heap)

     (deftype Heap
              [^Comparator compare-priority ^PriorityQueue buf]
       Counted
       (count [_] (.size buf))
       IPersistentStack
       (peek [_] (when-let [item (.peek buf)] [item (priority item)]))
       (pop [self] (.poll buf) self)
       (cons [self item] (.offer buf item) self)
       (empty [_] (make-heap compare-priority))
       (equiv [self other] (identical? self other))
       IEditableCollection
       (asTransient [self] self)
       ITransientCollection
       (conj [self item] (.offer buf item) self)
       (persistent [self] self)
       IMeta
       (meta [_] {})
       IObj
       (withMeta [self _] self))

     (defn make-heap
       [^Comparator compare-priority]
       (new Heap
            compare-priority
            (new PriorityQueue
                 ^Comparator (priority-comparator compare-priority)))))

   :cljs
   ;; CLJS: Priority map based implementation with unified interface
   (do
     (declare make-heap)

     (deftype Heap [compare-priority ^:mutable pmap]
       ICounted
       (-count [_] (count pmap))
       ISeqable
       (-seq [_] (seq pmap))
       ILookup
       (-lookup [_ k] (get pmap k))
       (-lookup [_ k not-found] (get pmap k not-found))
       IStack
       (-peek [_]
         (when-let [[item _] (first pmap)]
           [item (priority item)]))
       (-pop [self]
         (when-let [[item _] (first pmap)]
           (set! pmap (dissoc pmap item)))
         self)
       ICollection
       (-conj [self item]
         (set! pmap (assoc pmap item (priority item)))
         self)
       IEmptyableCollection
       (-empty [_] (make-heap compare-priority))
       IEquiv
       (-equiv [self other] (identical? self other))
       ITransientCollection
       (-conj! [self item]
         (set! pmap (assoc pmap item (priority item)))
         self)
       (-persistent! [self] self)
       IMeta
       (-meta [_] {})
       IWithMeta
       (-with-meta [self _] self))

     (defn make-heap [compare-priority]
       (Heap. compare-priority
              (pm/priority-map-by compare-priority)))))

(defn do-search [root-node config]
  (let [{search-xf :search-xf compare-priority :compare-priority} config]
    (loop [node root-node
           heap (make-heap compare-priority)]
      (let [children (children node)]
        (if-let [result# (stop node children)]
          result#
          (let [heap' (try (into heap search-xf children)
                           (catch TimeoutException _))
                head-node+priority (peek heap')]
            (if head-node+priority
              (let [next-node (combine (first head-node+priority) node)
                    next-heap (pop heap')]
                (recur next-node next-heap))
              node)))))))

(defn init-timeout-config! [{timeout :timeout :as config}]
  (if timeout
    (let [timeout? (timeout/make-timeout timeout)
          timeout-xf (map #(if (timeout?) (throw timeout-exception) %))]
      (-> config
          (update :search-xf #(comp timeout-xf %))))
    config))

(def DEFAULT-CONFIG
  {:root-node nil
   :search-xf IDENTITY-XFORM
   :compare-priority larger-is-better
   :timeout nil})

;; # API
(defn search
  [{timeout :timeout :as search-config}]
  (let [{:keys [root-node] :as complete-config} (cond-> (merge DEFAULT-CONFIG
                                                               search-config)
                                                  timeout init-timeout-config!)
        result (do-search root-node complete-config)]
    (if (reduced? result) @result result)))