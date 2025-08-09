(ns me.lomin.sinho.search
  ^{:doc "chatda (kor. 찾다, to search) is a library to search for stuff in parallel"}
  (:import (java.util.concurrent Executors)
           (java.util.concurrent TimeoutException TimeUnit ScheduledExecutorService)
           (clojure.lang IPersistentStack Counted IEditableCollection ITransientCollection IMeta IObj)
           (java.util PriorityQueue Comparator)))

#_(set! *warn-on-reflection* true)

(defonce timeout-executor
  (delay (Executors/newSingleThreadScheduledExecutor)))

(def IDENTITY-XFORM (map identity))

;; # Protocols

(defprotocol SearchableNode
  ;; Must return nil, an empty sequence or a sequence with items
  ;;of type SearchableNode.
  (children [self])
  ;; Must return a number value representing the priority of a SearchableNode.
  (priority [self])
  ;; There are two different ways to stop a search:
  ;; 1. `stop` return an object wrapped in `reduced`:
  ;; This will stop all workers immediately and return the object
  ;; 2. `stop` returns a truthy value:
  ;; This will stop the worker that returned the truthy value, but
  ;; different workers will still continue their search.
  (stop [this children])
  ;; Whenever a new SearchableNode is taken from the heap, it will be
  ;; combined with the previous top prioritized SearchableNode by calling
  ;; `(combine current previous)`.
  (combine [this previous]))

;; # Comparators

(defn priority-comparator [compare-priority]
  (fn [a b] (compare-priority (priority a) (priority b))))

(def smaller-is-better compare)

(def larger-is-better (fn [a b] (compare b a)))

;; # Heap

(declare make-heap)

;; DO NOT USE Heap OUTSIDE THIS NAMESPACE!
;; It does not properly comply to the contract of the implemented
;; protocols in favor of performance optimization. This Heap implementation brings
;; a performance increase of about 30% compared to `clojure.data.priority-map`.
;; The use of Heap in this namespace is intended to look like idiomatic Clojure code,
;; but if it is used other than the fine-tuned accesses in this namespace, the
;; abstraction will probably break.
;; Heap behaves like a mutable variable with the API of a transient, but without
;; its semantics. For example, `(persistent! heap)` will return the same heap.
;; Neither Heap nor its underlying java.util.PriorityQueue are synchronized.
;; This is fine in this namespace, since we guarantee that a Heap has only
;; exactly one accessor, i.e. we guarantee thread isolation.
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
  ([^Comparator compare-priority]
   (new Heap
        compare-priority
        (new PriorityQueue
             ^Comparator (priority-comparator compare-priority)))))

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

(defn search-sequential [{:keys [root-node] :as config}]
  (let [result (do-search root-node config)]
    (if (reduced? result)
      @result
      result)))

;; Creating TimeoutException ahead of time and only once, since
;; creating an exception is expensive and we are not interested
;; in the stacktrace. This significantly reduces the latency
;; between the moment of the timeout and the unblocking of the
;; main thread.
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
        (update :search-xf #(comp timeout-xf %)))))

(def DEFAULT-CONFIG
  {:root-node        nil
   :parallelism      1
   :chan-size        1
   :search-xf        IDENTITY-XFORM
   :compare-priority larger-is-better
   :timeout          nil
   ;; internal
   ::timeout-future  nil})

;; # API

(defn search [{timeout :timeout :as search-config}]
  (let [complete-config
        (cond-> (merge DEFAULT-CONFIG search-config)
          timeout init-timeout-config!)]
    (try
      (search-sequential complete-config)
      (finally
        (some-> complete-config ::timeout-future future-cancel)))))
