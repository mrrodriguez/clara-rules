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
            AccumulateWithJoinFilterNode]))
(defmacro dbgd [x] `(let [x# ~x] (println '~x) (prn x#) x#))

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
                                  [[fact-bindings result]]
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


;;;; Rulebase print-dup impl's.

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
         :accumulator (*compile-expr-fn* (:id node)
                                         (:accum-expr (meta node)))))

(defn node-id->node [node-id]
  (@*node-id->node-cache* node-id))

(defn cache-node [node]
  (if-let [node-id (:id node)]
    (do
      (vswap! *node-id->node-cache* assoc node-id node)
      node)
    node))

(def print-dup-record
  (get-method print-dup clojure.lang.IRecord))

(defn print-dup-node
  ([node ^java.io.Writer w]
   (print-dup-node nil node nil w))
  ([^String header
    node
    ^String footer
    ^java.io.Writer w]
   (let [node-id (:id node)]
     (if (@*node-id->node-cache* node-id)
       (do
         (.write w "#=(clara.rules.durability/node-id->node ")
         (print-dup node-id w)
         (.write w ")"))
       (do
         (cache-node node)
         (vswap! *node-id->node-cache* assoc node-id node)
         (.write w "#=(clara.rules.durability/cache-node ")
         (when header (.write w header))
         (print-dup-record node w)
         (when footer (.write w footer))
         (.write w ")")
         )))))

(defn- write-join-filter-node [node ^java.io.Writer w]
  (print-dup-node "#=(clara.rules.durability/add-join-filter-fn "
                  (assoc node :join-filter-fn nil)
                  ")"
                  w))

(defmethod print-dup ProductionNode [o ^java.io.Writer w]
  (print-dup-node "#=(clara.rules.durability/add-rhs-fn "
                  (assoc o :rhs nil)
                  ")"
                  w))

(defmethod print-dup QueryNode [o ^java.io.Writer w]
  (print-dup-node o w))

(defmethod print-dup AlphaNode [o ^java.io.Writer w]
  (.write w "#=(clara.rules.durability/add-alpha-fn ")
  ;; AlphaNode's do not have an :id
  (print-dup-record (assoc o :activation nil) w)
  (.write w ")"))

(defmethod print-dup RootJoinNode [o ^java.io.Writer w]
  (print-dup-node o w))

(defmethod print-dup HashJoinNode [o ^java.io.Writer w]
  (print-dup-node o w))

(defmethod print-dup ExpressionJoinNode [o ^java.io.Writer w]
  (write-join-filter-node o w))

(defmethod print-dup NegationNode [o ^java.io.Writer w]
  (print-dup-node o w))

(defmethod print-dup NegationWithJoinFilterNode [o ^java.io.Writer w]
  (write-join-filter-node o w))

(defmethod print-dup TestNode [o ^java.io.Writer w]
  (print-dup-node "#=(clara.rules.durability/add-test-fn "
                  (assoc o :test nil)
                  ")"
                  w))

(defmethod print-dup AccumulateNode [o ^java.io.Writer w]
  (print-dup-node "#=(clara.rules.durability/add-accumulator "
                  (assoc o :accumulator nil)
                  ")"
                  w))

(defmethod print-dup AccumulateWithJoinFilterNode [o ^java.io.Writer w]
  (print-dup-node "#=(clara.rules.durability/add-accumulator #=(clara.rules.durability/add-join-filter-fn "
                  (assoc o :accumulator nil :join-filter-fn nil)
                  "))"
                  w))

(defmethod print-dup RuleOrderedActivation [^RuleOrderedActivation o ^java.io.Writer w]
  (.write w "#")
  (.write w (.getName (class o)))
  (print-dup [(.-node-id o)
              (.-token o)
              (.-activation o)
              (.-rule-load-order o)]
             w))

;;;; Store and restore functions.

(defn store-rulebase-state-to [session out]
  (let [{:keys [rulebase]} (eng/components session)]
    (binding [*node-id->node-cache* (volatile! {})
              *print-meta* true
              *print-dup* true
              *out* (jio/writer out)]
      (pr {:rulebase rulebase})
      (flush))))

(defn store-session-state-to [session out opts]
  (let [{:keys [rulebase memory]} (eng/components session)
        memory-state (select-keys memory
                                  #{:alpha-memory
                                    :beta-memory
                                    :accum-memory
                                    :production-memory
                                    :activation-map})
        to-store (cond-> {:memory memory-state}
                   (:with-rulebase? opts) (assoc :rulebase rulebase))]
    (binding [*node-id->node-cache* (volatile! {})
              *print-meta* true
              *print-dup* true
              *out* (jio/writer out)]
      (pr to-store)
      (flush))))

(defn restore-rulebase-state-from [in]
  (binding [*node-id->node-cache* (volatile! {})
            *compile-expr-fn* (memoize (fn [id expr] (com/try-eval expr)))]
    (-> in
        jio/reader
        clojure.lang.LineNumberingPushbackReader.
        ;; One item expected in the stream.  Read it.  Fail if nothing found.
        read
        :rulebase)))

(defn restore-session-state-from [in opts]
  (let [{:keys [base-session listeners transport]} opts
        session-state (binding [*node-id->node-cache* (volatile! {})
                                *compile-expr-fn* (memoize (fn [id expr] (com/try-eval expr)))]
                        (-> in
                            jio/reader
                            clojure.lang.LineNumberingPushbackReader.
                            ;; One item expected in the stream.  Read it.  Fail if nothing found.
                            read))
        ;; The rulebase should either be given from the base-session or found in
        ;; the restored session-state.
        rulebase (or (some-> base-session eng/components :rulebase)
                     (:rulebase session-state))
        opts (-> opts
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
        memory (-> (:memory session-state)
                   (merge memory-opts)
                   ;; Naming difference for some reason.
                   (set/rename-keys {:get-alphas-fn :alphas-fn})
                   mem/map->PersistentLocalMemory)
        ;; (eng/init-memory rulebase
        ;;                  (-> (:memory session-state)
        ;;                      (merge memory-opts)
        ;;                      ;; Naming difference for some reason.
        ;;                      (set/rename-keys {:get-alphas-fn :alphas-fn})
        ;;                      mem/map->PersistentLocalMemory)
        ;;                  transport)
        ]
    (eng/assemble {:rulebase rulebase
                   :memory memory
                   :transport transport
                   :listeners listeners
                   :get-alphas-fn get-alphas-fn})))
