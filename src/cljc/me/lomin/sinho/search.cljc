(ns me.lomin.sinho.search
  ^{:doc "chatda (kor. 찾다, to search) is a library to search for stuff in parallel"}
  (:require [me.lomin.sinho.timeout :as timeout]
            #?@(:bb [[clojure.data.priority-map :refer [priority-map priority-map-keyfn]]]
                :cljs [[tailrecursion.priority-map :as pm]]))
  #?@(:bb []
      :clj [(:import (java.util.concurrent TimeoutException)
                     (clojure.lang IPersistentStack Counted IEditableCollection
                                   ITransientCollection IMeta IObj)
                     (java.util PriorityQueue Comparator))]))

#?(:clj (set! *warn-on-reflection* true))

(def IDENTITY-XFORM (map identity))

#?(:cljs
   (deftype TimeoutException [message]
     Object
     (toString [_] message)))

;; Pre-create exception for performance
(def timeout-exception #?(:bb (ex-info "Timeout" {:type ::timeout})
                          :clj (TimeoutException. "Timeout")
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

#?(:bb
   ;; BB: Priority map based implementation (no deftype in Babashka)
   ;; Uses BB's built-in priority-map (ascending order = smaller-is-better).
   (do
     (defn make-heap [compare-priority]
       (if (neg? (compare-priority 1 0))
         (priority-map-keyfn -)
         (priority-map)))

     (defn heap-into [heap search-xf items]
       (into heap
             (comp search-xf (map (fn [item] [item (priority item)])))
             items)))

   :clj
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
                 ^Comparator (priority-comparator compare-priority))))

     (defn heap-into [heap search-xf items]
       (into heap search-xf items)))

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
              (pm/priority-map-by compare-priority)))

     (defn heap-into [heap search-xf items]
       (into heap search-xf items))))

(defn do-search [root-node config]
  (let [{search-xf :search-xf compare-priority :compare-priority} config]
    (loop [node root-node
           heap (make-heap compare-priority)]
      (let [children (children node)]
        (if-let [result# (stop node children)]
          result#
          (let [heap' (try (heap-into heap search-xf children)
                           (catch #?(:bb Exception
                                     :clj TimeoutException
                                     :cljs TimeoutException) _))
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
   :compare-priority larger-is-better ; must be a comparator over numeric priorities (tested with (compare-priority 1 0) on BB)
   :timeout 1000 ; 1 second default timeout
   })

;; # API
(defn search
  [search-config]
  (let [{:keys [root-node] :as complete-config}
        (init-timeout-config! (merge DEFAULT-CONFIG search-config))
        result (do-search root-node complete-config)]
    (if (reduced? result) @result result)))