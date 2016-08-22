(ns clara.rules.durability
  "Support for persisting Clara sessions to an external store.
   Provideds the ability to store and restore an entire session working memory state.  The restored
   session is able to have additional insert, retract, query, and fire rule calls performed 
   immediately after.

   See https://github.com/rbrush/clara-rules/issues/198 for more discussion on this.

   Note! This is still an EXPERIMENTAL namespace. This may change non-passively without warning.
   Any session or rulebase serialized in one version of Clara is not guaranteed to deserialize 
   successfully against another version of Clara."
  (:require [clara.rules.engine :as eng]
            [clara.rules.compiler :as com]
            [clara.rules.memory :as mem]
            [clojure.set :as set]
            [schema.core :as s])
  (:import [clara.rules.compiler
            Rulebase]
           [clara.rules.memory
            RuleOrderedActivation]
           [java.util
            List
            Map
            IdentityHashMap]))

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

;;; TODO Share from clara.rules.memory potentially.
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

(defn index-alpha-memory [seen amem]
  (let [index-update-bindings-fn #(index-bindings seen %)
        index-update-elements (fn [elements]
                                (mapv (fn [elem]
                                        (-> elem
                                            (update :fact
                                                    #(find-index-or-add! seen %))
                                            (update :bindings
                                                    index-update-bindings-fn)))
                                      elements))]
    (update-vals amem
                 #(-> (index-update-bindings-keys index-update-bindings-fn %)
                      (update-vals index-update-elements)))))

(defn index-accum-memory [seen accum-mem]
  (let [index-update-bindings-fn #(index-bindings seen %)
        index-facts (fn [facts]
                      (mapv #(find-index-or-add! seen %) facts))
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
                                                (find-index-or-add! seen res))]
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

(defn index-beta-memory [seen bmem]
  (let [index-update-tokens (fn [tokens]
                              (mapv #(index-token seen %) tokens))]
    (update-vals bmem
                 (fn [v]
                   (-> (index-update-bindings-keys #(index-bindings seen %) v)
                       (update-vals index-update-tokens))))))

(defn index-production-memory [seen pmem]
  (let [index-update-facts (fn [facts]
                             (mapv #(or (find-index seen %)
                                        (find-index-or-add! seen %))
                                   facts))]
    (update-vals pmem
                 (fn [token-map]
                   (->> token-map
                        (reduce-kv (fn [m k v]
                                     (assoc! m
                                             (index-token seen k)
                                             (mapv index-update-facts v)))
                                   (transient {}))
                        persistent!)))))

(defn index-activation-map [seen actmap]
  (update-vals actmap
               #(mapv (fn [^RuleOrderedActivation act]
                        (mem/->RuleOrderedActivation (.-node-id act)
                                                     (index-token seen (.-token act))
                                                     (.-activation act)
                                                     (.-rule-load-order act)))
                      %)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Commonly useful session serialization helpers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private ^:dynamic *mem-facts* nil)

(defn find-mem-idx [idx]
  (get *mem-facts* idx))

(defn indexed-session-memory-state
  "Takes the working memory from a session and strips it down to only the memory needed for
   serialization.  Along with this, replaces all working memory facts with MemIdx place holders.
   The terminology being used here is to call this step 'indexing' the memory.  
   
   A map is returned with two keys:
   * :memory - The working memory representation that is the same as the given memory's :memory,
     however, all facts in the memory are replaced with MemIdx placeholders. 
   * :indexed-facts - the facts replaced with MemIdx placeholders.  The facts are returned in a
     sequential collection.  Each fact is the n'th item of the collection if the MemIdx for that
     fact has :idx = n.  No facts returned should be identical? (i.e. multiple references to the
     same object instance).  However, it is possible for some facts returned to be aggregations
     containing other facts that do appear elsewhere in the fact sequence.  It is up to the
     implementation of the IWorkingMemorySerializer to deal with these possible, identical? object
     references correctly.  This is generally true for most serialization mechanisms.

   Note!  This function should not typically be used.  It is left public to assist in ISessionSerializer
          durability implementations.  Use clara.rules/mk-session typically to make rule sessions."
  [memory]
  (let [vec-indexed-facts (fn [^java.util.Map fact->index-map]
                            ;; It is not generally safe to reduce or seq over a mutable Java Map.
                            ;; One example is IdentityHashMap.  The iterator of the IdentityHashMap
                            ;; mutates the map entry values in place and it is never safe to call a
                            ;; Iteratore.hasNext() when not finished working with the previous value
                            ;; returned from Iteratore.next().  This is subtle and is actually only a
                            ;; problem in JDK6 for IdentityHashMap.  JDK7+ appear to have discontinued
                            ;; this mutable map entry.  However, this is not something to rely on and
                            ;; JDK6 support is still expected to work for Clara.  The only trasducer
                            ;; in Clojure that can currently, safely consume the JDK6-style
                            ;; IdentityHashMap via its entry set iterator is Eduction.  This doesn't
                            ;; appear to be due to explicit semantics though, but rather an
                            ;; implementation detail.
                            ;; For further context, this situation is related, but not exactly the
                            ;; same as http://dev.clojure.org/jira/browse/CLJ-1738.
                            (let [;; The use of a primitive array here isn't strictly necessary.  However,
                                  ;; it doesn't add much in terms of complexity and is faster than the
                                  ;; alternatives.
                                  ^"[Ljava.lang.Object;" arr (make-array Object (.size fact->index-map))
                                  es (.entrySet fact->index-map)
                                  it (.iterator es)]
                              (when (.hasNext it)
                                (loop [^java.util.Map$Entry e (.next it)]
                                  (aset arr (:idx (.getValue e)) ^Object (.getKey e))
                                  (when (.hasNext it)
                                    (recur (.next it)))))
                              (into [] arr)))

        index-memory (fn [memory]
                       (let [seen (java.util.IdentityHashMap.)

                             indexed (-> memory
                                         (update :accum-memory #(index-accum-memory seen %))
                                         (update :alpha-memory #(index-alpha-memory seen %))
                                         (update :beta-memory #(index-beta-memory seen %))
                                         (update :production-memory #(index-production-memory seen %))
                                         (update :activation-map #(index-activation-map seen %)))]

                         {:memory indexed
                          :indexed-facts (vec-indexed-facts seen)}))]
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
                :alphas-fn))))

(defn assemble-restored-session
  "Builds a Clara session from the given rulebase and memory components.
   Note!  This function should not typically be used.  It is left public to assist in ISessionSerializer 
          durability implementations.  Use clara.rules/mk-session typically to make rule sessions.
   
   Options can be provided via opts.
   These include:

   * :activation-group-sort-fn 
   * :activation-group-fn
   * :get-alphas-fn 

   If the options are not provided, they will default to the Clara session defaults.
   These are all described in detail in clara.rules/mk-session docs."
  [rulebase memory opts]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Serialization protocols.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ISessionSerializer
  "Provides the ability to serialize and deserialize a session.  Options can be given and supported
   via the opts argument to both serialize and deserialize.  Certain options are expected and 
   required to be supported by any implementation of ISessionSerializer.  These are referred to as 
   the 'standard' options. 
   
   These include:

   * :rulebase-only? - When true indicates the rulebase is the only part of the session to serializer.
     The *default* is false for the serialize-session-state function.  It is defaulted to true for the
     serialize-rulebase convenience function.  This is useful for when many sessions are to be
     serialized, but all having a common rulebase.  Storing the rulebase only, will likely save both
     space and time in these scenarios.

   * :with-rulebase? - When true the rulebase is included in the serialized state of the session.  
     The *default* behavior is false when serializing a session via the serialize-session-state function.

   * :base-rulebase - A rulebase to attach to the session being deserialized.  The assumption here is that
     the session was serialized without the rulebase, i.e. :with-rulebase? = false, so it needs a rulebase
     to be 'attached' back onto it to be usable.

   Other options can be supported by specific implementors of ISessionSerializer."

  (serialize [this session opts]
    "Serialize the given session with the given options.  Where the session state is stored is dependent
     on the implementation of this instance e.g. it may store it in a known reference to an IO stream.")

  (deserialize [this mem-facts opts]
    "Deserialize the session state associated to this instance e.g. it may be coming from a known reference
     to an IO stream.  mem-facts is a sequential collection of the working memory facts that were 
     serialized and deserialized by an implementation of IWorkingMemorySerializer."))

(defprotocol IWorkingMemorySerializer
  "Provides the ability to serialize and deserialize the facts stored in the working memory of a session.
   Facts can be serialized in whatever way makes sense for a given domain.  The domain of facts can vary
   greatly from one use-case of the rules engine to the next.  So the mechanism of serializing the facts
   in memory can vary greatly as a result of this.  Clara does not yet provide any default implementations
   for this, but may in the future.  However, many of the handlers defined in clara.rules.durability.fressian
   can be reused if the consumer wishes to serialize via Fressian.  See more on this in 
   the clara.rules.durability.fressian namespace docs.
   
   The important part of this serialization protocol is that the facts returned from deserialize-facts are in
   the *same order* as how they were given to serialize-facts."
  
  (serialize-facts [this fact-seq]
    "Serialize the given fact-seq, which is an order sequence of facts from working memory of a session.  
     Note, as mentioned in the protocol docs, the *order* these are given is *important* and should be preserved
     when they are returned via deserialize-facts.")

  (deserialize-facts [this]
    "Returns the facts associated to this instance deserialized in the same order that they were given
     to serialize-facts."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Durability API.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/defn serialize-rulebase
  "Serialize *only* the rulebase portion of the given session.  The serialization is done by the
   given session-serializer implementor of ISessionSerializer.  

   Options can be given as an optional argument.  These are passed through to the session-serializer
   implementation.  See the description of standard options an ISessionSerializer should provide in
   the ISessionSerializer docs.  Also, see the specific ISessionSerializer implementation docs for 
   any non-standard options supported/not supported.
   See ISessionSerializer docs for more on that.

   The rulebase is the stateless structure that controls the flow of productions, i.e. the 'rete'
   rule network.  The ability to serialize only the rulebase is supported so that the rulebase can
   be stored and retrieved a single time for potentially many sessions containing different working
   memory data, for the same rules.  This function is only a convenience for passing the
   :rulebase-only? true flag to the serialize-session-state function.
   See serialize-session-state for more." 
  ([session :- (s/protocol eng/ISession)
    session-serializer :- (s/protocol ISessionSerializer)]
   (serialize-rulebase session
                       session-serializer
                       {}))

  ([session :- (s/protocol eng/ISession)
    session-serializer :- (s/protocol ISessionSerializer)
    opts :- {s/Any s/Any}]
   (serialize session-serializer
              session
              (assoc opts :rulebase-only? true))))

(s/defn deserialize-rulebase :- Rulebase
  "Deserializes the rulebase stored via the serialize-rulebase function.  This is done via the given
   session-serializer implementor of ISessionSerializer.

   Options can be given as an optional argument.  These are passed through to the session-serializer
   implementation.  See the description of standard options an ISessionSerializer should provide in
   the ISessionSerializer docs.  Also, see the specific ISessionSerializer implementation docs for 
   any non-standard options supported/not supported.
   See ISessionSerializer docs for more on that."
  ([session-serializer :- (s/protocol ISessionSerializer)]
   (deserialize-rulebase session-serializer
                         {}))

  ([session-serializer :- (s/protocol ISessionSerializer)
    opts :- {s/Any s/Any}]
   (deserialize session-serializer
                nil
                (assoc opts :rulebase-only? true))))

(s/defn serialize-session-state
  "Serializes the state of the given session.  By default, this *excludes* the rulebase from being
   serialized alongside the working memory state of the session.  The rulebase, if specified, and 
   the working memory of the session are serialized by the session-serializer implementor of 
   ISessionSerializer.  The memory-serializer implementor of IWorkingMemorySerializer is used to
   serialize the actual facts stored within working memory.  

   Typically, the caller can use a pre-defined default session-serializer, such as
   clara.rules.durability.fressian/create-session-serializer.  
   See clara.rules.durability.fressian for more specific details regarding this, including the extra
   required dependency on Fressian notes found there.
   The memory-facts-serializer is often a custom provided implemenation since the facts stored in
   working memory are domain specific to the consumers' usage of the rules.
   See the IWorkingMemorySerializer docs for more.

   Options can be given as an optional argument.  These are passed through to the session-serializer
   implementation.  See the description of standard options an ISessionSerializer should provide in
   the ISessionSerializer docs.  Also, see the specific ISessionSerializer implementation docs for 
   any non-standard options supported/not supported."
  ([session :- (s/protocol eng/ISession)
    session-serializer :- (s/protocol ISessionSerializer)
    memory-facts-serializer :- (s/protocol IWorkingMemorySerializer)]
   (serialize-session-state session
                            session-serializer
                            memory-facts-serializer
                            {:with-rulebase? false}))

  ([session :- (s/protocol eng/ISession)
    session-serializer :- (s/protocol ISessionSerializer)
    memory-facts-serializer :- (s/protocol IWorkingMemorySerializer)
    opts :- {s/Any s/Any}]
   (serialize-facts memory-facts-serializer
                    (serialize session-serializer session opts))))

(s/defn deserialize-session-state :- (s/protocol eng/ISession)
  "Deserializes the session that was stored via the serialize-session-state function.  Similar to
   what is described there, this uses the session-serializer implementor of ISessionSerializer to
   deserialize the session and working memory state.  The memory-facts-serializer implementor of 
   IWorkingMemorySerializer is used to deserialize the actual facts stored in working memory.

   Options can be given as an optional argument.  These are passed through to the session-serializer
   implementation.  See the description of standard options an ISessionSerializer should provide in
   the ISessionSerializer docs.  Also, see the specific ISessionSerializer implementation docs for 
   any non-standard options supported/not supported."
  ([session-serializer :- (s/protocol ISessionSerializer)
    memory-facts-serializer :- (s/protocol IWorkingMemorySerializer)]
   (deserialize-session-state session-serializer
                              memory-facts-serializer
                              nil))
  
  ([session-serializer :- (s/protocol ISessionSerializer)
    memory-facts-serializer :- (s/protocol IWorkingMemorySerializer)
    opts :- {s/Any s/Any}]
   (deserialize session-serializer
                (deserialize-facts memory-facts-serializer)
                opts)))
