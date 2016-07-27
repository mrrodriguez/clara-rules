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
            IdentityHashMap]))
(def holder (atom {}))
(def times (atom 0))
(defmacro dbgd [x] `(let [x# ~x] (when (< @times 40) (println '~x) (prn x#)) (swap! times inc) (swap! holder assoc '~x x#) x#))
(println "COMPILING DURABILITY AGAIN")

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


;;;; Rulebase print-method impl's.

(def ^:dynamic *node-id->node-cache* nil)

(def ^:dynamic *compile-expr-fn*
  nil)

(defn- add-node-fn [node fn-key meta-key]
  (assoc node
         fn-key
         (*compile-expr-fn* (:id node) (meta-key (meta node)))))

(defn add-rhs-fn [node]
  (add-node-fn node :rhs :action-expr))

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

(def ^:private print-meta @#'clojure.core/print-meta)
(def ^:private print-map @#'clojure.core/print-map)

(def ^:private ^:dynamic *cached-records?* false)
(def ^:private ^:dynamic *clj-record-holder* nil)

(defn clj-record-id->fact [id]
  (.get *clj-record-holder* id))

(defn clj-record-fact-holder-add! [x]
  (.put *clj-record-holder* x (.size *clj-record-holder*))
  x)

(defn print-method-record [o ^java.io.Writer w]
  (let [print-record (fn []
                       (let [class-name (-> o class .getName)
                             idx (.lastIndexOf class-name (int \.))
                             ns-nom (.substring class-name 0 idx)
                             nom (.substring class-name (inc idx))
                             builder-name (symbol (str (cm/demunge ns-nom) "/map->" (cm/demunge nom)))]
                         (print-meta o w)
                         (.write w (str "#=(" builder-name))
                         (print-map o print-method w)
                         (.write w ")")))]

    (if *cached-records?*
      (if-let [idx (some-> *clj-record-holder* (.get o))]
        (do
          (.write w "#=(clara.rules.durability/clj-record-id->fact ")
          (print-method idx w)
          (.write w ")"))
        
        (do
          (.write w "#=(clara.rules.durability/clj-record-fact-holder-add! ")
          (print-record)
          (.write w ")")
          
          (.put *clj-record-holder* o (.size *clj-record-holder*))))
      (print-record))))

(defn print-method-node
  ([node ^java.io.Writer w]
   (print-method-node nil node nil w))
  ([^String header
    node
    ^String footer
    ^java.io.Writer w]
   (let [node-id (:id node)]
     (if (@*node-id->node-cache* node-id)
       (do
         (.write w "#=(clara.rules.durability/node-id->node ")
         (print-method node-id w)
         (.write w ")"))
       (do
         (cache-node node)
         (vswap! *node-id->node-cache* assoc node-id node)
         (.write w "#=(clara.rules.durability/cache-node ")
         (when header (.write w header))
         (print-method-record node w)
         (when footer (.write w footer))
         (.write w ")")
         )))))

(defn- write-join-filter-node [node ^java.io.Writer w]
  (print-method-node "#=(clara.rules.durability/add-join-filter-fn "
                  (assoc node :join-filter-fn nil)
                  ")"
                  w))

(defmethod print-method clojure.lang.IRecord [o ^java.io.Writer w]
  (print-method-record o w))

(defmethod print-method ProductionNode [o ^java.io.Writer w]
  (print-method-node "#=(clara.rules.durability/add-rhs-fn "
                     (assoc o :rhs nil)
                     ")"
                     w))

(defmethod print-method QueryNode [o ^java.io.Writer w]
  (print-method-node o w))

(defmethod print-method AlphaNode [o ^java.io.Writer w]
  (.write w "#=(clara.rules.durability/add-alpha-fn ")
  ;; AlphaNode's do not have an :id
  (print-method-record (assoc o :activation nil) w)
  (.write w ")"))

(defmethod print-method RootJoinNode [o ^java.io.Writer w]
  (print-method-node o w))

(defmethod print-method HashJoinNode [o ^java.io.Writer w]
  (print-method-node o w))

(defmethod print-method ExpressionJoinNode [o ^java.io.Writer w]
  (write-join-filter-node o w))

(defmethod print-method NegationNode [o ^java.io.Writer w]
  (print-method-node o w))

(defmethod print-method NegationWithJoinFilterNode [o ^java.io.Writer w]
  (write-join-filter-node o w))

(defmethod print-method TestNode [o ^java.io.Writer w]
  (print-method-node "#=(clara.rules.durability/add-test-fn "
                  (assoc o :test nil)
                  ")"
                  w))

(defmethod print-method AccumulateNode [o ^java.io.Writer w]
  (print-method-node "#=(clara.rules.durability/add-accumulator "
                  (assoc o :accumulator nil)
                  ")"
                  w))

(defmethod print-method AccumulateWithJoinFilterNode [o ^java.io.Writer w]
  (print-method-node "#=(clara.rules.durability/add-accumulator #=(clara.rules.durability/add-join-filter-fn "
                  (assoc o :accumulator nil :join-filter-fn nil)
                  "))"
                  w))

(defmethod print-method RuleOrderedActivation [^RuleOrderedActivation o ^java.io.Writer w]
  (.write w "#")
  (.write w (.getName (class o)))
  (print-method [(.-node-id o)
              (.-token o)
              (.-activation o)
              (.-rule-load-order o)]
             w))

;;;; Memory serialization

(defrecord MemIdx [idx])
(defrecord AccumResultMemIdx [idx])

(defn find-index
  [^java.util.Map fact->index-map fact]
  (.get fact->index-map fact))

(defn- find-index-or-add!
  ([^java.util.Map fact->index-map fact]
   (find-index-or-add! fact->index-map fact false))
  ([^java.util.Map fact->index-map fact accum-result?]
   (or (.get fact->index-map fact)
       (let [n (.size fact->index-map)
             idx (if accum-result?
                   (->AccumResultMemIdx n)
                   (->MemIdx n))]
         (.put fact->index-map fact idx)
         idx))))

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
  ([seen bindings]
   (index-bindings seen nil bindings))
  ([seen accum-results bindings]
   (update-vals bindings
                #(or (when accum-results (find-index accum-results %))
                     (find-index seen %)
                     %))))

(defn- unindex-bindings
  ([indexed bindings]
   (unindex-bindings indexed nil bindings))
  ([indexed accum-results bindings]
   (update-vals bindings
                #(cond
                   (and accum-results (instance? AccumResultMemIdx %))
                   (nth accum-results (:idx %))
                   
                   (instance? MemIdx %)
                   (nth indexed (:idx %))
                   
                   :else
                   %))))

(defn- index-update-bindings-keys [index-update-bindings-fn
                                   bindings-map]
  (persistent!
   (reduce-kv (fn [m k v]
                (assoc! m
                        (index-update-bindings-fn k)
                        v))
              (transient {})
              bindings-map)))

(defn- index-token [seen accum-results token]
  (-> token
      (update :matches
              #(mapv (fn [[fact node-id]]
                       [(or (find-index accum-results fact)
                            (find-index-or-add! seen fact))
                        node-id])
                     %))
      (update :bindings
              #(index-bindings seen accum-results %))))

(defn- unindex-token [indexed accum-results token]
  (-> token
      (update :matches
              #(vec
                (for [[idx node-id] %]
                  [(if (instance? AccumResultMemIdx idx)
                     (nth accum-results (:idx idx))
                     (nth indexed (:idx idx)))
                   node-id])))
      (update :bindings
              #(unindex-bindings indexed accum-results %))))

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
                                  accum-result-update-fn
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
                                                (accum-result-update-fn node-id facts res))]
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

(defrecord AccumResultStub [node-id facts])

(defn index-accum-memory [seen accum-results accum-mem]
  (update-accum-memory-index #(find-index-or-add! seen %)
                             #(index-bindings seen accum-results %)
                             (fn [node-id facts res]
                               (find-index-or-add! accum-results res true)
                               (->AccumResultStub node-id facts))
                             accum-mem))

(defn unindex-accum-memory [node-id->accumulator indexed accum-results accum-mem]
  (update-accum-memory-index #(nth indexed (:idx %))
                             #(unindex-bindings indexed accum-results %)
                             (fn [node-id facts _]
                               (let [{:keys [initial-value reduce-fn]} (node-id->accumulator node-id)
                                     res (reduce reduce-fn initial-value facts)]
                                 (vswap! accum-results conj res)
                                 res))
                             accum-mem))

(defn- update-beta-memory-index [index-update-fn
                                 index-update-bindings-fn
                                 bmem]
  (let [index-update-tokens #(mapv index-update-fn %)]
    (update-vals bmem
                 #(-> (index-update-bindings-keys index-update-bindings-fn %)
                      (update-vals index-update-tokens)))))

(defn index-beta-memory [seen accum-results bmem]
  (update-beta-memory-index #(index-token seen accum-results %)
                            #(index-bindings seen accum-results %)
                            bmem))

(defn unindex-beta-memory [indexed accum-results bmem]
  (update-beta-memory-index #(unindex-token indexed accum-results %)
                            #(unindex-bindings indexed accum-results %)
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

(defn index-production-memory [seen accum-results pmem]
  (update-production-memory-index #(or (find-index accum-results %)
                                       (find-index-or-add! seen %))
                                  #(index-token seen accum-results %)
                                  pmem))

(defn unindex-production-memory [indexed accum-results pmem]
  (update-production-memory-index #(if (instance? AccumResultMemIdx %)
                                     (nth accum-results (:idx %))
                                     (nth indexed (:idx %)))
                                  #(unindex-token indexed accum-results %)
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

(defn index-activation-map [seen accum-results actmap]
  (update-activation-map-index #(index-token seen accum-results %) actmap))

(defn unindex-activation-map [indexed accum-results actmap]
  (update-activation-map-index #(unindex-token indexed accum-results %) actmap))

(defn index-memory [memory]
  (let [seen (java.util.IdentityHashMap.)
        ;; Only mutable during conversion of the :accumulate-memory.  After that only used to replace
        ;; binding values with indices to recover later.
        accum-results (java.util.IdentityHashMap.)

        indexed (-> memory
                   (update :accum-memory #(index-accum-memory seen accum-results %))
                   (update :alpha-memory #(index-alpha-memory seen %))
                   (update :beta-memory #(index-beta-memory seen accum-results %))
                   (update :production-memory #(index-production-memory seen accum-results %))
                   (update :activation-map #(index-activation-map seen accum-results %)))]

    {:memory indexed
     :indexed-facts (vec-indexed-facts seen)}))

(defn unindex-memory [indexed-facts rulebase memory]
  (let [;; Needed to replay accumulator results...
        node-id->accumulator
        (into {}
              (comp (filter (comp #(or (instance? AccumulateNode %)
                                       (instance? AccumulateWithJoinFilterNode %))
                                  val))
                    (map (fn [[node-id node]] [node-id (:accumulator node)])))
              (:id-to-node rulebase))

        ;; Only mutable during conversion of the :accumulate-memory.
        accum-results (volatile! [])
        memory (-> memory
                   (update :alpha-memory #(unindex-alpha-memory indexed-facts %))
                   (update :accum-memory #(unindex-accum-memory node-id->accumulator indexed-facts accum-results %)))
        accum-results @accum-results]

    (-> memory
        (update :beta-memory #(unindex-beta-memory indexed-facts accum-results %))
        (update :production-memory #(unindex-production-memory indexed-facts accum-results %))
        (update :activation-map #(unindex-activation-map indexed-facts accum-results %)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Serialization protocols.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ISessionSerializer
  (serialize [this session opts])
  (deserialize [this mem-facts opts]))

(defprotocol IMemoryFactsSerializer
  (serialize-facts [this fact-seq opts])
  (deserialize-facts [this opts]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Basic print-method based session serializer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private ^:dynamic *mem-facts* nil)
(def ^:private ^:dynamic *node-id->accumulator* nil)
(def ^:private ^:dynamic *accum-results* nil)

(defn find-mem-idx [idx]
  (get *mem-facts* idx))

(defn find-accum-result-mem-idx [idx]
  (get @*accum-results* idx))

(defn run-accum [accum-result-stub]
  (let [{:keys [node-id facts]} accum-result-stub
        {:keys [initial-value reduce-fn]} (*node-id->accumulator* node-id)
        res (reduce reduce-fn initial-value facts)]
    (vswap! *accum-results* conj res)
    res))

;;;; To deal with http://dev.clojure.org/jira/browse/CLJ-1733 we need to impl print-method on
;;;; sorted sets and maps.  However, this is not sufficient for arbitrary comparators.  If
;;;; arbitrary comparators are used for the sorted coll, the comparator has to be restored
;;;; explicitly since arbitrary functions are not serializable in any stable way right now.

(defn- print-method-sorted [create-fn-str seq-fn ^clojure.lang.Sorted s ^java.io.Writer w]
  (let [cname (-> s meta :print-method/comparator-name)]

    ;; Fail if reliable serialization of this sorted coll isn't possible.
    (when (and (not cname)
               (not= (.comparator s) clojure.lang.RT/DEFAULT_COMPARATOR))
      (throw (ex-info (str "Cannot serialize (via print-method) sorted collection with non-default"
                           " comparator because no :print-method/comparator-name provided in metadata.")
                      {:sorted-coll s
                       :comparator (.comparator s)})))
    
    (.write w (str "#=(" create-fn-str " "))
    (when cname
      (.write w "#=(deref #=(var ")
      (print-method cname w)
      (.write w ") )"))
    ;; The seq of a sorted coll may returns entry types that do not have a valid print-method
    ;; representation.  To workaround that, replace them all with 2 item vectors instead.
    (print-method (seq-fn s) w)
    (.write w ")")))

(defmethod print-method clojure.lang.PersistentTreeSet [o ^java.io.Writer w]
  (print-method-sorted "clojure.lang.PersistentTreeSet/create" seq o w))

(defmethod print-method clojure.lang.PersistentTreeMap [o ^java.io.Writer w]
  (print-method-sorted "clojure.lang.PersistentTreeMap/create" #(sequence cat %) o w))

(defmethod print-method MemIdx [o ^java.io.Writer w]
  (.write w "#=(clara.rules.durability/find-mem-idx ")
  (print-method (:idx o) w)
  (.write w ")"))

(defmethod print-method AccumResultStub [o ^java.io.Writer w]
  (.write w "#=(clara.rules.durability/run-accum ")
  (print-method-record o w)
  (.write w ")"))

(defmethod print-method AccumResultMemIdx [o ^java.io.Writer w]
  (.write w "#=(clara.rules.durability/find-accum-result-mem-idx ")
  (print-method (:idx o) w)
  (.write w ")"))

(defrecord PrintMethodSessionSerializer [in-stream out-stream]
  ISessionSerializer
  (serialize [_ session opts]
    (let [{:keys [rulebase memory]} (eng/components session)
          record-holder (IdentityHashMap.)
          do-serialize (fn [print-method-sources]
                         (with-open [wtr (jio/writer in-stream)]
                           (binding [*node-id->node-cache* (volatile! {})
                                     *clj-record-holder* record-holder
                                     *cached-records?* true
                                     *print-meta* true
                                     *print-readably* true
                                     *out* wtr]
                             (doseq [prd print-method-sources] (pr prd))
                             (flush))))]

      ;; In this case there is nothing to do with memory, so just serialize immediately.
      (if (:rulebase-only? opts)
        (do-serialize [rulebase])

        ;; Otherwise memory needs to have facts extracted to return.
        (let [imem (index-memory memory)
              ;; Technically the rulebase is in memory too currently.
              session-state (-> imem
                                (assoc :rulebase rulebase)
                                (dissoc :indexed-facts)
                                (update :memory
                                        dissoc
                                        ;; The rulebase does need to be stored per memory.  It will be restored during deserialization.
                                        :rulebase
                                        ;; Currently these do not support serialization and must be provided during deserialization via a
                                        ;; base-session or they default to the standard defaults.
                                        :activation-group-sort-fn
                                        :activation-group-fn
                                        :alphas-fn))
              memory (:memory session-state)
              print-method-sources (cond
                                     (:with-rulebase? opts) [rulebase memory]
                                     :else [memory])]

          (do-serialize print-method-sources)

          ;; Return the facts needing to be serialized still.
          (:indexed-facts imem)))))

  (deserialize [_ mem-facts opts]
    (with-open [rdr (clojure.lang.LineNumberingPushbackReader. (jio/reader out-stream))]
      (let [{:keys [rulebase-only? base-rulebase listeners transport]} opts
            record-holder (IdentityHashMap.)
            ;; The rulebase should either be given from the base-session or found in
            ;; the restored session-state.
            rulebase (or base-rulebase
                         (binding [*node-id->node-cache* (volatile! {})
                                   *clj-record-holder* record-holder
                                   *compile-expr-fn* (memoize (fn [id expr] (com/try-eval expr)))]
                           (read rdr)))]
        (if rulebase-only?
          rulebase

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

                memory-opts (select-keys opts
                                         #{:rulebase
                                           :activation-group-sort-fn
                                           :activation-group-fn
                                           :get-alphas-fn})

                transport (or transport (clara.rules.engine.LocalTransport.))
                listeners (or listeners [])
                get-alphas-fn (:get-alphas-fn opts)

                accum-node? #(or (instance? AccumulateNode %)
                                 (instance? AccumulateWithJoinFilterNode %))

                node-id->accumulator (into {}
                                           (comp (filter (comp accum-node? val))
                                                 (map (fn [[node-id node]] [node-id (:accumulator node)])))
                                           (:id-to-node rulebase))

                memory (-> (binding [*node-id->accumulator* node-id->accumulator
                                     *clj-record-holder* record-holder
                                     *mem-facts* mem-facts
                                     *accum-results* (volatile! [])]
                             (read rdr))
                           ;; Memory currently maintains its own reference to the rulebase.  This was removed when serializing, but should be put back now.
                           (assoc :rulebase rulebase)
                           (merge memory-opts)
                           ;; Naming difference for some reason.
                           (set/rename-keys {:get-alphas-fn :alphas-fn})
                           mem/map->PersistentLocalMemory)]

            (eng/assemble {:rulebase rulebase
                           :memory memory
                           :transport transport
                           :listeners listeners
                           :get-alphas-fn get-alphas-fn})))))))

;;; TODO remove?
(defrecord InMemoryMemoryFactsSerializer [holder]
  IMemoryFactsSerializer
  (serialize-facts [this fact-seq opts]
    (reset! holder fact-seq))
  (deserialize-facts [_ opts]
    @holder))

(defn create-default-session-serializer
  ([in+out-stream]
   (create-default-session-serializer in+out-stream in+out-stream))
  ([in-stream out-stream]
   (->PrintMethodSessionSerializer in-stream out-stream)))

(defn serialize-rulebase
  ([session session-serializer]
   (serialize-rulebase session
                       session-serializer))
  ([session session-serializer opts]
   (serialize session-serializer
              session
              (assoc opts :rulebase-only? true))))

(defn deserialize-rulebase
  ([session-serializer]
   (deserialize-rulebase session-serializer))
  ([session-serializer opts]
   (deserialize session-serializer
                nil
                (assoc opts :rulebase-only? true))))

(defn serialize-session-state
  ([session session-serializer memory-facts-serializer]
   (serialize-session-state session session-serializer memory-facts-serializer nil))
  ([session session-serializer memory-facts-serializer opts]
   (serialize-facts memory-facts-serializer
                    (serialize session-serializer session opts)
                    opts)))

(defn deserialize-session-state
  ([session-serializer memory-facts-serializer]
   (deserialize-session-state session-serializer
                              memory-facts-serializer
                              nil))
  ([session-serializer memory-facts-serializer opts]
   (deserialize session-serializer
                (deserialize-facts memory-facts-serializer opts)
                opts)))
