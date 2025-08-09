(ns me.lomin.sinho.search
  ^{:doc "chatda (kor. 찾다, to search) is a library to search for stuff in parallel"}
  #?(:clj
     (:import (java.util.concurrent Executors TimeoutException TimeUnit ScheduledExecutorService)
              (clojure.lang IPersistentStack Counted IEditableCollection 
                           ITransientCollection IMeta IObj)
              (java.util PriorityQueue Comparator)))
  #?(:cljs
     (:require [tailrecursion.priority-map :as pm])))

#?(:clj (set! *warn-on-reflection* true))

;; Timeout executor - CLJ only
#?(:clj
   (defonce timeout-executor
     (delay (Executors/newSingleThreadScheduledExecutor))))

(def IDENTITY-XFORM (map identity))

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
              (pm/priority-map-by 
                (fn [a b] (compare-priority (priority a) (priority b))))))))

(defn do-search [root-node config]
  (let [{search-xf :search-xf compare-priority :compare-priority} config]
    (loop [node root-node
           heap (make-heap compare-priority)]
      (let [children (children node)]
        (if-let [result# (stop node children)]
          result#
          (let [heap' (try (into heap search-xf children)
                           #?(:clj (catch TimeoutException _)
                              :cljs (catch :default _)))
                head-node+priority (peek heap')]
            (if head-node+priority
              (let [next-node (combine (first head-node+priority) node)
                    next-heap (pop heap')]
                (recur next-node next-heap))
              node)))))))

(defn search-sequential [{:keys [root-node] :as config}]
  (let [result (do-search root-node config)]
    (if (reduced? result)
      @result
      result)))

;; # Timeout implementation - CLJ only

#?(:clj
   (do
     ;; Pre-create exception for performance
     (def timeout-exception (new TimeoutException))
     
     (defn init-timeout-config! [{timeout :timeout :as config}]
       (let [timed-out? (volatile! false)
             timeout-xf (map #(if @timed-out? (throw timeout-exception) %))]
         (-> config
             (assoc ::timeout-future
                    (.schedule ^ScheduledExecutorService @timeout-executor
                              ^Runnable #(do (vreset! timed-out? true))
                              ^long timeout
                              TimeUnit/MILLISECONDS))
             (update :search-xf #(comp timeout-xf %))))))
   
   :cljs
   ;; CLJS: No-op timeout implementation
   (defn init-timeout-config! [config]
     config))

(def DEFAULT-CONFIG
  {:root-node        nil
   :parallelism      1
   :chan-size        1
   :search-xf        IDENTITY-XFORM
   :compare-priority larger-is-better
   :timeout          nil
   ;; internal
   #?@(:clj [::timeout-future nil])})

;; # API
(defn search [{timeout :timeout :as search-config}]
  (let [complete-config
        (cond-> (merge DEFAULT-CONFIG search-config)
          timeout init-timeout-config!)]
    #?(:clj
       (try
         (search-sequential complete-config)
         (finally
           (some-> complete-config ::timeout-future future-cancel)))
       :cljs
       (search-sequential complete-config))))