(ns clara.rules.durability
  "Experimental namespace. This may change non-passively without warning.

   Support for persisting Clara sessions to an external store. This will have two models: obtaining the full
   state of a session, and obtaining a \"diff\" of changes from a previous state.

   See the session-state function to retrieve the full state of a session as a data structure that can
   be easily serialized via EDN or Fressian. Sessions can then be recovered with the restore-session-state function.

   TODO: diff support is pending -- functions for obtaining a diff of state from a previous point, allowing for a write-ahead log."
  (:require [clara.rules :refer :all]
            [clara.rules.listener :as l]
            [clara.rules.engine :as eng]
            [clara.rules.compiler :as com]
            [clara.rules.memory :as mem]
            [clojure.java.io :as jio]
            [clojure.set :as set]
            [clojure.main :as cm]
            [schema.core :as s]
            [schema.macros :as sm])

  (:import [clara.rules.memory
            RuleOrderedActivation]
           [clara.rules.engine
            Token
            ProductionNode
            QueryNode
            AlphaNode
            RootJoinNode
            HashJoinNode
            ExpressionJoinNode
            NegationNode
            NegationWithJoinFilterNode
            TestNode
            AccumulateNode
            AccumulateWithJoinFilterNode]
           [java.util
            List
            Map
            IdentityHashMap]))

(def times (atom 0))
(defmacro dbgd [x]
  `(let [x# ~x]
     (when (< @times 1e10) (println '~x);; (prn (type x#))
           ;;(prn (hash x#))
           (prn x#))
     (swap! times inc)
     x#))

;; A schema representing a minimal representation of a rule session's state.
;; This allows for efficient storage, particularly when serialized with Fressian or a similar format
;; to eliminate redundnacy.
(def session-state-schema
  {
   ;; Map of matching facts to the number of them that matched.
   :fact-counts {s/Any s/Int}

   ;; Map of node IDs to tokens indicating a pending rule activation.
   :activations {s/Int [Token]}

   ;; Map of accumulator node IDs to accumulated results.
   :accum-results {s/Int [{:join-bindings {s/Keyword s/Any} :fact-bindings {s/Keyword s/Any} :result s/Any}]}

   ;; Map associating node ids and tokens with a vector of facts they had inserted. This is used
   ;; to track truth maintenance, so if a given token is retracted, we can also retract the inferred items.
   :insertions {[(s/one s/Int "node-id") (s/one Token "token")] [[s/Any]]}})

(s/defn session-state :- session-state-schema
  " Returns the state of a session as an EDN- or Fressian-serializable data structure. The returned
   structure contains only the minimal data necessary to reconstruct the session via the restore-session-state
   function below."
  [session]
  (let [{:keys [rulebase memory]} (eng/components session)
        {:keys [id-to-node production-nodes query-nodes]} rulebase
        beta-nodes (for [[id node] id-to-node
                         :when (or (instance? HashJoinNode node)
                                   (instance? RootJoinNode node)
                                   (instance? ExpressionJoinNode node))]
                     node)

        accumulate-nodes (for [[id node] id-to-node
                         :when (instance? AccumulateNode node)]
                     node)

        ;; Get the counts for each beta node. Mutliple nodes may have the same facts
        ;; but merging these is benign since the counts would also match.
        fact-counts (reduce
                     (fn [fact-counts beta-node]
                       (let [facts (for [{:keys [fact]} (mem/get-elements-all memory beta-node)]
                                     fact)]
                         (merge fact-counts (frequencies facts))))
                     {}
                     beta-nodes)

        activations (->> (for [{:keys [node token]} (mem/get-activations memory)]
                           {(:id node) [token]})
                         (apply merge-with concat))

        accum-results (into {}
                            (for [accum-node accumulate-nodes]
                                 [(:id accum-node) (mem/get-accum-reduced-complete memory accum-node)]))

        insertions (into {}
                         (for [[id node] id-to-node
                               :when (instance? ProductionNode node)
                               token (keys (mem/get-insertions-all memory node))]
                           [[(:id node) token] (mem/get-insertions memory node token)] ))]

    {:fact-counts fact-counts
     :activations (or activations {})
     :accum-results (or accum-results {})
     :insertions insertions}))

(defn- restore-activations
  "Restores the activations to the given session."
  [session {:keys [activations] :as session-state}]
  (let [{:keys [memory rulebase] :as components} (eng/components session)
        {:keys [production-nodes id-to-node]} rulebase

        restored-activations (for [[node-id tokens] activations
                                   token tokens]
                               (eng/->Activation (id-to-node node-id) token))

        grouped-by-production (group-by (fn [activation]
                                          (-> activation :node :production)) restored-activations)

        transient-memory (doto (mem/to-transient memory)
                           (mem/clear-activations!))]

    (doseq [[production activations] grouped-by-production]

      (mem/add-activations! transient-memory production activations))

    ;; Create a new session with the given activations.
    (eng/assemble (assoc components :memory (mem/to-persistent! transient-memory)))))

(defn- restore-accum-results
  [session {:keys [accum-results] :as session-state}]
  (let [{:keys [memory rulebase transport] :as components} (eng/components session)
        id-to-node (:id-to-node rulebase)
        transient-memory (mem/to-transient memory)]

    ;; Add the results to the accumulator node.
    (doseq [[id results] accum-results
            {:keys [join-bindings fact-bindings result]} results]

      (eng/right-activate-reduced (id-to-node id)
                                  join-bindings
                                  [[fact-bindings (first result)]]
                                  transient-memory
                                  transport
                                  (l/to-transient l/default-listener)))

    (eng/assemble (assoc components :memory (mem/to-persistent! transient-memory)))))

(defn- restore-insertions
  [session {:keys [insertions] :as session-state}]
  (let [{:keys [memory rulebase transport] :as components} (eng/components session)
        id-to-node (:id-to-node rulebase)
        transient-memory (mem/to-transient memory)]

    ;; Add the results to the accumulator node.
    (doseq [[[id token] inserted-facts] insertions
            insertion-group inserted-facts]
      ;; Each insertion group represents a distinct activation of the ProductionNode.
      (mem/add-insertions! transient-memory (id-to-node id) token insertion-group))

    (eng/assemble (assoc components :memory (mem/to-persistent! transient-memory)))))

(s/defn restore-session-state
  " Restore the given session to have the provided session state. The given session should be
   a newly-created session that was created with the same parameters as the session that was
   serialized. For instance, it should use the same rulesets, type function, and other settings.

   This function returns a new session instance that reflects the given saved state.  The returned
   session should be indistinguishable from the session that had its state saved."
  [session
   {:keys [fact-counts] :as session-state} :- session-state-schema]
  (let [fact-seq (for [[fact count] fact-counts
                       i (range count)]
                   fact)]

    (-> session
        (insert-all fact-seq)
        (restore-activations session-state)
        (restore-accum-results session-state)
        (restore-insertions session-state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Restoring an entire active session in memory that is able to insert, retract, and fire rule again to obtain new working memory states.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rulebase serialization helpers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *node-id->node-cache* nil)

(def ^:dynamic *compile-expr-fn*
  nil)

(defn- add-node-fn [node fn-key meta-key]
  (assoc node
         fn-key
         (*compile-expr-fn* (:id node) (meta-key (meta node)))))

(defn add-rhs-fn [node]
  (with-bindings (if-let [ns (some-> node
                                     :production
                                     :ns-name
                                     the-ns)]
                   {#'*ns* ns}
                   {})
    (add-node-fn node :rhs :action-expr)))

(defn add-alpha-fn [node]
  ;; AlphaNode's do not have node :id's right now since they don't
  ;; have any memory specifically associated with them.
  (assoc node :activation (com/try-eval (:alpha-expr (meta node)))))

(defn add-join-filter-fn [node]
  (add-node-fn node :join-filter-fn :join-filter-expr))

(defn add-test-fn [node]
  (add-node-fn node :test :test-expr))

(defn add-accumulator [node]
  (assoc node
         :accumulator ((*compile-expr-fn* (:id node)
                                          (:accum-expr (meta node)))
                       (:env node))))

(defn node-id->node [node-id]
  (@*node-id->node-cache* node-id))

(defn cache-node [node]
  (if-let [node-id (:id node)]
    (do
      (vswap! *node-id->node-cache* assoc node-id node)
      node)
    node))

;;; TODO this is print-method specific so should be moved there.
(def ^:dynamic *serializing-session*
  "When this is true, any Clara durability specific `print-method` impl's 
   will be activated.  This may clash with any other `print-method` impl's
   currently in use by the caller.  When this is false, `print-method`
   will ignore any specific Clara `print-method` impl's."
  false)

(def ^:dynamic *clj-record-holder*
  "A cache for writing and reading Clojure records.  At write time, an IdentityHashMap can be
   used to keep track of repeated references to the same record object instance occurring in
   the serialization stream.  At read time, a plain ArrayList (mutable and indexed for speed)
   can be used to add records to when they are first seen, then look up repeated occurrences
   of references to the same record instance later."
  nil)

(defn clj-record-fact->idx [fact]
  (.get ^Map *clj-record-holder* fact))

(defn clj-record-holder-add-fact-idx! [fact]
  ;; Note the values will be int type here.  This shouldn't be a problem since they
  ;; will be read later as longs and both will be compatible with the index lookup
  ;; at read-time.  This could have a cast to long here, but it would waste time
  ;; unnecessarily.
  (.put ^Map *clj-record-holder* fact (.size ^Map *clj-record-holder*)))

(defn clj-record-idx->fact [id]
  (.get ^List *clj-record-holder* id))

(defn clj-record-holder-add-fact! [fact]
  (.add ^List *clj-record-holder* fact)
  fact)

(defn create-map-entry [k v]
  ;; Using the ctor instead of clojure.lang.MapEntry/create since this method
  ;; doesn't exist prior to clj 1.8.0
  (clojure.lang.MapEntry. k v))

;;;; To deal with http://dev.clojure.org/jira/browse/CLJ-1733 we need to impl a way to serialize
;;;; sorted sets and maps.  However, this is not sufficient for arbitrary comparators.  If
;;;; arbitrary comparators are used for the sorted coll, the comparator has to be restored
;;;; explicitly since arbitrary functions are not serializable in any stable way right now.

(defn sorted-comparator-name [^clojure.lang.Sorted s]
  (let [cname (-> s meta ::comparator-name)]

    ;; Fail if reliable serialization of this sorted coll isn't possible.
    (when (and (not cname)
               (not= (.comparator s) clojure.lang.RT/DEFAULT_COMPARATOR))
      (throw (ex-info (str "Cannot serialize (via print-method) sorted collection with non-default"
                           " comparator because no :print-method/comparator-name provided in metadata.")
                      {:sorted-coll s
                       :comparator (.comparator s)})))

    cname))

(defn seq->sorted-set
  [s ^java.util.Comparator c]
  (if c
    (clojure.lang.PersistentTreeSet/create c (seq s))
    (clojure.lang.PersistentTreeSet/create (seq s))))

(defn seq->sorted-map
  [s ^java.util.Comparator c]
  (if c
    (clojure.lang.PersistentTreeMap/create c ^clojure.lang.ISeq (sequence cat s))
    (clojure.lang.PersistentTreeMap/create ^clojure.lang.ISeq (sequence cat s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Memory serialization via "indexing" working memory facts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord MemIdx [idx])

(defn find-index
  [^java.util.Map fact->index-map fact]
  (.get fact->index-map fact))

(defn- find-index-or-add!
  [^java.util.Map fact->index-map fact]
  (or (.get fact->index-map fact)
      (let [n (.size fact->index-map)
            idx (->MemIdx n)]
        (.put fact->index-map fact idx)
        idx)))

(defn vec-indexed-facts [^java.util.Map fact->index-map]
  (let [^"[Ljava.lang.Object;" arr (make-array Object (.size fact->index-map))
        es (.entrySet fact->index-map)
        it (.iterator es)]
    (when (.hasNext it)
      (loop [^java.util.Map$Entry e (.next it)]
        (aset arr (:idx (.getValue e)) ^Object (.getKey e))
        (when (.hasNext it)
          (recur (.next it)))))
    (into [] arr)))

;;; TODO Share from clara.rules.memory?
;;; TODO is it faster to start from an empty map or from a transient copy of m?
(defn- update-vals [m update-fn]
  (->> m
       (reduce-kv (fn [m k v]
                    (assoc! m k (update-fn v)))
                  (transient {}))
       persistent!))

(defn- index-bindings
  [seen bindings]
  (update-vals bindings
               #(or (find-index seen %)
                    %)))

(defn- unindex-bindings
  [indexed bindings]
  (update-vals bindings
               #(if (instance? MemIdx %)
                  (nth indexed (:idx %))
                  %)))

(defn- index-update-bindings-keys [index-update-bindings-fn
                                   bindings-map]
  (persistent!
   (reduce-kv (fn [m k v]
                (assoc! m
                        (index-update-bindings-fn k)
                        v))
              (transient {})
              bindings-map)))

(defn- index-token [seen token]
  (-> token
      (update :matches
              #(mapv (fn [[fact node-id]]
                       [(find-index-or-add! seen fact)
                        node-id])
                     %))
      (update :bindings
              #(index-bindings seen %))))

(defn- unindex-token [indexed token]
  (-> token
      (update :matches
              #(vec
                (for [[idx node-id] %]
                  [(nth indexed (:idx idx))
                   node-id])))
      (update :bindings
              #(unindex-bindings indexed %))))

(defn- update-alpha-memory-index [index-update-fact-fn
                                  index-update-bindings-fn
                                  amem]
  (let [index-update-elements (fn [elements]
                                (mapv #(-> %
                                           (update :fact
                                                   index-update-fact-fn)
                                           (update :bindings
                                                   index-update-bindings-fn))
                                      elements))]
    (update-vals amem
                 #(-> (index-update-bindings-keys index-update-bindings-fn %)
                      (update-vals index-update-elements)))))

(defn index-alpha-memory [seen amem]
  (update-alpha-memory-index #(find-index-or-add! seen %)
                             #(index-bindings seen %)
                             amem))

(defn unindex-alpha-memory [indexed amem]
  (update-alpha-memory-index #(nth indexed (:idx %))
                             #(unindex-bindings indexed %)
                             amem))

(defn- update-accum-memory-index [index-update-fn
                                  index-update-bindings-fn
                                  accum-mem]
  (let [index-facts #(mapv index-update-fn %)
        index-update-accum-reduced (fn [node-id accum-reduced]
                                     (let [m (meta accum-reduced)]
                                       (if (::eng/accum-node m)
                                         ;; AccumulateNode
                                         (let [[facts res] accum-reduced
                                               facts (index-facts facts)]
                                           (with-meta
                                             [facts
                                              (if (= ::eng/not-reduced res)
                                                res
                                                (index-update-fn res))]
                                             m))

                                         ;; AccumulateWithJoinFilterNode
                                         (with-meta (index-facts accum-reduced)
                                           m))))
        index-update-bindings-map (fn [node-id bindings-map]
                                    (-> (index-update-bindings-keys index-update-bindings-fn bindings-map)
                                        (update-vals #(index-update-accum-reduced node-id %))))]

    (->> accum-mem
         (reduce-kv (fn [m node-id bindings-map]
                      (assoc! m node-id (-> (index-update-bindings-keys index-update-bindings-fn bindings-map)
                                            (update-vals #(index-update-bindings-map node-id %)))))
                    (transient {}))
         persistent!)))

(defn index-accum-memory [seen accum-mem]
  (update-accum-memory-index #(find-index-or-add! seen %)
                             #(index-bindings seen %)
                             accum-mem))

(defn unindex-accum-memory [indexed accum-mem]
  (update-accum-memory-index #(nth indexed (:idx %))
                             #(unindex-bindings indexed %)
                             accum-mem))

(defn- update-beta-memory-index [index-update-fn
                                 index-update-bindings-fn
                                 bmem]
  (let [index-update-tokens #(mapv index-update-fn %)]
    (update-vals bmem
                 #(-> (index-update-bindings-keys index-update-bindings-fn %)
                      (update-vals index-update-tokens)))))

(defn index-beta-memory [seen bmem]
  (update-beta-memory-index #(index-token seen %)
                            #(index-bindings seen %)
                            bmem))

(defn unindex-beta-memory [indexed bmem]
  (update-beta-memory-index #(unindex-token indexed %)
                            #(unindex-bindings indexed %)
                            bmem))

(defn update-production-memory-index [index-update-fact-fn
                                      index-update-token-fn
                                      pmem]
  (let [index-update-facts #(mapv index-update-fact-fn %)]
    (update-vals pmem
                 (fn [token-map]
                   (->> token-map
                        (reduce-kv (fn [m k v]
                                     (assoc! m
                                             (index-update-token-fn k)
                                             (mapv index-update-facts v)))
                                   (transient {}))
                        persistent!)))))

(defn index-production-memory [seen pmem]
  (update-production-memory-index #(or (find-index seen %)
                                       (find-index-or-add! seen %))
                                  #(index-token seen %)
                                  pmem))

(defn unindex-production-memory [indexed pmem]
  (update-production-memory-index #(nth indexed (:idx %))
                                  #(unindex-token indexed %)
                                  pmem))

;;; TODO can there be duplicate activations to share instead of copy?
(defn- update-activation-map-index [index-update-fn actmap]
  (update-vals actmap
               #(mapv (fn [^RuleOrderedActivation act]
                        (mem/->RuleOrderedActivation (.-node-id act)
                                                     (index-update-fn (.-token act))
                                                     (.-activation act)
                                                     (.-rule-load-order act)))
                      %)))

(defn index-activation-map [seen actmap]
  (update-activation-map-index #(index-token seen %) actmap))

(defn unindex-activation-map [indexed actmap]
  (update-activation-map-index #(unindex-token indexed %) actmap))

(defn index-memory [memory]
  (let [seen (java.util.IdentityHashMap.)

        indexed (-> memory
                   (update :accum-memory #(index-accum-memory seen %))
                   (update :alpha-memory #(index-alpha-memory seen %))
                   (update :beta-memory #(index-beta-memory seen %))
                   (update :production-memory #(index-production-memory seen %))
                   (update :activation-map #(index-activation-map seen %)))]

    {:memory indexed
     :indexed-facts (vec-indexed-facts seen)}))

(defn unindex-memory [indexed-facts rulebase memory]
  (let [memory (-> memory
                   (update :alpha-memory #(unindex-alpha-memory indexed-facts %))
                   (update :accum-memory #(unindex-accum-memory indexed-facts %)))]

    (-> memory
        (update :beta-memory #(unindex-beta-memory indexed-facts %))
        (update :production-memory #(unindex-production-memory indexed-facts %))
        (update :activation-map #(unindex-activation-map indexed-facts %)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Serialization protocols.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ISessionSerializer
  (serialize [this session opts])
  (deserialize [this mem-facts opts]))

(defprotocol IWorkingMemorySerializer
  (serialize-facts [this fact-seq])
  (deserialize-facts [this]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Basic print-method based session serializer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private ^:dynamic *mem-facts* nil)

(defn find-mem-idx [idx]
  (get *mem-facts* idx))


(defn session-memory-state [memory]
  (-> memory
      index-memory
      (update :memory
              dissoc
              ;; The rulebase does need to be stored per memory.  It will be restored during deserialization.
              :rulebase
              ;; Currently these do not support serialization and must be provided during deserialization via a
              ;; base-session or they default to the standard defaults.
              :activation-group-sort-fn
              :activation-group-fn
              :alphas-fn)))

(defn assemble-restored-session [rulebase memory opts]
  (let [opts (-> opts
                 (assoc :rulebase rulebase)
                 ;; Right now activation fns do not serialize.
                 (update :activation-group-sort-fn
                         #(eng/options->activation-group-sort-fn {:activation-group-sort-fn %}))
                 (update :activation-group-fn
                         #(eng/options->activation-group-fn {:activation-group-fn %}))
                 ;; TODO: Memory doesn't seem to ever need this or use it.  Can we just remove it from memory?
                 (update :get-alphas-fn
                         #(or % (@#'com/create-get-alphas-fn type ancestors rulebase))))

        {:keys [listeners transport get-alphas-fn]} opts
        
        memory-opts (select-keys opts
                                 #{:rulebase
                                   :activation-group-sort-fn
                                   :activation-group-fn
                                   :get-alphas-fn})

        transport (or transport (clara.rules.engine.LocalTransport.))
        listeners (or listeners [])

        memory (-> memory
                   (merge memory-opts)
                   ;; Naming difference for some reason.
                   (set/rename-keys {:get-alphas-fn :alphas-fn})
                   mem/map->PersistentLocalMemory)]
    
    (eng/assemble {:rulebase rulebase
                   :memory memory
                   :transport transport
                   :listeners listeners
                   :get-alphas-fn get-alphas-fn})))

(defn serialize-rulebase
  ([session session-serializer]
   (serialize-rulebase session
                       session-serializer
                       {}))
  ([session session-serializer opts]
   (serialize session-serializer
              session
              (assoc opts :rulebase-only? true))))

(defn deserialize-rulebase
  ([session-serializer]
   (deserialize-rulebase session-serializer
                         {}))
  ([session-serializer opts]
   (deserialize session-serializer
                nil
                (assoc opts :rulebase-only? true))))

(defn serialize-session-state
  ([session session-serializer memory-facts-serializer]
   (serialize-session-state session
                            session-serializer
                            memory-facts-serializer
                            {:with-rulebase? false}))
  ([session session-serializer memory-facts-serializer opts]
   (serialize-facts memory-facts-serializer
                    (serialize session-serializer session opts))))

(defn deserialize-session-state
  ([session-serializer memory-facts-serializer]
   (deserialize-session-state session-serializer
                              memory-facts-serializer
                              nil))
  ([session-serializer memory-facts-serializer opts]
   (deserialize session-serializer
                (deserialize-facts memory-facts-serializer)
                opts)))
