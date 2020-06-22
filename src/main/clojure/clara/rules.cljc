(ns clara.rules
  "Forward-chaining rules for Clojure. The primary API is in this namespace."
  (:require [clara.rules.engine :as eng]
            [clara.rules.cljs :as cljs]
            [schema.core :as s]
            [clara.rules.platform :as platform]
            #?(:cljs [clara.rules.listener :as l])
            #?(:clj [clara.rules.compiler :as com])
            #?(:clj [clara.rules.dsl :as dsl]))
  #?(:cljs (:require-macros clara.rules)))

(defn insert
  "Inserts one or more facts into a working session. It does not modify the given
   session, but returns a new session with the facts added."
  [session & facts]
  (eng/insert session facts))

(defn insert-all
  "Inserts a sequence of facts into a working session. It does not modify the given
   session, but returns a new session with the facts added."
  [session fact-seq]
  (eng/insert session fact-seq))

(defn retract
  "Retracts a fact from a working session. It does not modify the given session,
   but returns a new session with the facts retracted."
  [session & facts]
  (eng/retract session facts))

(defn fire-rules
  "Fires are rules in the given session. Once a rule is fired, it is labeled in a fired
   state and will not be re-fired unless facts affecting the rule are added or retracted.

   This function does not modify the given session to mark rules as fired. Instead, it returns
   a new session in which the rules are marked as fired.

   This take an additional map of options as a second argument.  Current options:

   :cancelling true (EXPERIMENTAL, subject to change/removal.  Not supported in ClojureScript.):  
    Simultaneously propagate insertions and retractions through the rules network, at every step using the insertion and retractions of equals facts to cancel each
    other out and avoid operations deeper in the rules network.  The behavior of unconditional insertions and RHS (right-hand side) retractions
    is undefined when this option is enabled and this option should not be used when calling fire-rules can result in these operations.
    Note that this is purely a performance optimization and no guarantees are made at this time on whether a given rule's RHS will be called.
    When this option is used rule RHS code that is executed shouldn't do anything that impacts state other than perform logical insertions."
  ([session] (eng/fire-rules session {}))
  ([session opts] (eng/fire-rules session opts)))

(defn query
  "Runs the given query with the optional given parameters against the session.
   The optional parameters should be in map form. For example, a query call might be:

   (query session get-by-last-name :last-name \"Jones\")

   The query itself may be either the var created by a defquery statement,
   or the actual name of the query.
   "
  [session query & params]
  (eng/query session query (apply hash-map params)))

(defn insert!
  "To be executed within a rule's right-hand side, this inserts a new fact or facts into working memory.

   Inserted facts are logical, in that if the support for the insertion is removed, the fact
   will automatically be retracted. For instance, if there is a rule that inserts a \"Cold\" fact
   if a \"Temperature\" fact is below a threshold, and the \"Temperature\" fact that triggered
   the rule is retracted, the \"Cold\" fact the rule inserted is also retracted. This is the underlying
   truth maintenance facillity.

   This truth maintenance is also transitive: if a rule depends on some criteria to fire, and a
   criterion becomes invalid, it may retract facts that invalidate other rules, which in turn
   retract their conclusions. This way we can ensure that information inferred by rules is always
   in a consistent state."
  [& facts]
  (eng/insert-facts! facts false))

(defn insert-all!
  "Behaves the same as insert!, but accepts a sequence of facts to be inserted. This can be simpler and more efficient for
   rules needing to insert multiple facts.

   See the doc in insert! for details on insert behavior.."
  [facts]
  (eng/insert-facts! facts false))

(defn insert-unconditional!
  "To be executed within a rule's right-hand side, this inserts a new fact or facts into working memory.

   This differs from insert! in that it is unconditional. The facts inserted will not be retracted
   even if the rule activation doing the insert becomes false.  Most users should prefer the simple insert!
   function as described above, but this function is available for use cases that don't wish to use
   Clara's truth maintenance."
  [& facts]
  (eng/insert-facts! facts true))

(defn insert-all-unconditional!
  "Behaves the same as insert-unconditional!, but accepts a sequence of facts to be inserted rather than individual facts.

   See the doc in insert-unconditional! for details on uncondotional insert behavior."
  [facts]
  (eng/insert-facts! facts true))

(defn retract!
  "To be executed within a rule's right-hand side, this retracts a fact or facts from the working memory.

  Retracting facts from the right-hand side has slightly different semantics than insertion. As described
  in the insert! documentation, inserts are logical and will automatically be retracted if the rule
  that inserted them becomes false. This retract! function does not follow the inverse; retracted items
  are simply removed, and not re-added if the rule that retracted them becomes false.

  The reason for this is that retractions remove information from the knowledge base, and doing truth
  maintenance over retractions would require holding onto all retracted items, which would be an issue
  in some use cases. This retract! method is included to help with certain use cases, but unless you
  have a specific need, it is better to simply do inserts on the rule's right-hand side, and let
  Clara's underlying truth maintenance retract inserted items if their support becomes false."
  [& facts]
  (eng/rhs-retract-facts! facts))

(defn accumulate
  "DEPRECATED. Use clara.rules.accumulators/accum instead.

  Creates a new accumulator based on the given properties:

   * An initial-value to be used with the reduced operations.
   * A reduce-fn that can be used with the Clojure Reducers library to reduce items.
   * An optional combine-fn that can be used with the Clojure Reducers library to combine reduced items.
   * An optional retract-fn that can remove a retracted fact from a previously reduced computation
   * An optional convert-return-fn that converts the reduced data into something useful to the caller.
     Simply uses identity by default.
    "
  [& {:keys [initial-value reduce-fn combine-fn retract-fn convert-return-fn] :as args}]
  (eng/map->Accumulator
   (merge {;; Default conversion does nothing, so use identity.
           :convert-return-fn identity}
          args)))

#?(:clj
   (defn safe-resolve
     "Attempts to resolve symbol `sym`. Returns the result of `resolve` unless an exception is
  thrown, in which case returns nil."
     [sym]
     (if-not (platform/compiling-cljs?)
       (try
         (resolve sym)
         (catch Exception e
           nil))
       ;; TODO: need the value not the symbol.
       (cljs/resolve-cljs-sym (cljs/cljs-ns) sym))))

#?(:clj
   (defn resolve-symbol-rule-source
     "Find the rules and queries in the namespace, shred them, and compile them into a rule base."
     [sym]
     (let [cur-ns (ns-name *ns*)
           cur-cljs-ns (cljs/cljs-ns)
           resolved (safe-resolve sym)]
       (sc.api/spy sym)
       (cond
         ;; The symbol resolves to something, so it doesn't represent a namespace. Try to load rules
         ;; from what it resolves to.
         (some? resolved)
         (cond
           ;; The symbol references a rule or query, so just return it
           (or (:query (meta resolved))
               (:rule (meta resolved))) [@resolved]

           ;; The symbol references a sequence, so return it.
           (sequential? @resolved) @resolved

           :default
           (throw (ex-info (str "The source referenced by " sym " is not valid.") {:sym sym} )))

         ;; The symbol represents a namespace, so load rules from there.
         (and (not (namespace sym))
              (find-ns sym))
         (->> (ns-interns sym)
              (vals)                    ; Get the references in the namespace.
              (filter var?)
              (filter (comp (some-fn :rule :query :production-seq) meta)) ; Filter down to rules, queries, and seqs of both.
              ;; If definitions are created dynamically (i.e. are not reflected in an actual code file)
              ;; it is possible that they won't have :line metadata, so we have a default of 0.
              (sort (fn [v1 v2]
                      (compare (or (:line (meta v1)) 0)
                               (or (:line (meta v2)) 0))))
              (mapcat #(if (:production-seq (meta %))
                         (deref %)
                         [(deref %)])))
         ;; Failure.
         :else
         (throw (ex-info (str "Unable to resolve rule source: " sym)
                         {:sym sym}))))))

#?(:clj
   (defn resolve-object-rule-source
     "If `o` is a non-map collection that contains symbols, those symbols are resolved via
  `resolve-symbol-rule-source` and replaced with what they resolved to. Otherwise, `o` is directly
  returned."
     [o]
     ;; Try to find any sort of non-map collection, such as sequences, vectors, sets, etc.
     (if (and (not (map? o))
              (coll? o))
       (into (empty o)
             (mapcat #(if (symbol? %)
                        (resolve-symbol-rule-source %)
                        [%]))
             o)
       o)))

#?(:clj
   (extend-protocol com/IRuleSource
     clojure.lang.Symbol
     (load-rules [sym]
       (resolve-symbol-rule-source sym))
     java.lang.Object
     (load-rules [o]
       (resolve-object-rule-source o))))

#?(:clj
  (defmacro mk-session
     "Creates a new session using the given rule sources. The resulting session
      is immutable, and can be used with insert, retract, fire-rules, and query functions.

      If no sources are provided, it will attempt to load rules from the caller's namespace,
      which is determined by reading Clojure's *ns* var.

      This will use rules defined with defrule, queries defined with defquery, and sequences
      of rule and/or query structures in vars that are annotated with the metadata ^:production-seq.

      The caller may also specify keyword-style options at the end of the parameters. Currently five
      options are supported, although most users will either not need these or just the first two:

      * :fact-type-fn, which must have a value of a function used to determine the logical type of a given
        fact. Defaults to Clojure's type function.
      * :cache, indicating whether the session creation can be cached, effectively memoizing mk-session.
        Defaults to true. Callers may wish to set this to false when needing to dynamically reload rules.
      * :ancestors-fn, which returns a collection of ancestors for a given type. Defaults to Clojure's ancestors function. A
        fact of a given type will match any rule that uses one of that type's ancestors.  Note that if the collection is ordered
        this ordering will be maintained by Clara; ordering the ancestors consistently will increase the consistency of overall performance.
      * :activation-group-fn, a function applied to production structures and returns the group they should be activated with.
        It defaults to checking the :salience property, or 0 if none exists.
      * :activation-group-sort-fn, a comparator function used to sort the values returned by the above :activation-group-fn.
        Defaults to >, so rules with a higher salience are executed first.
      * :forms-per-eval - The maximum number of expressions that will be evaluated per call to eval.
        Larger batch sizes should see better performance compared to smaller batch sizes. (Only applicable to Clojure)
        Defaults to 5000, see clara.rules.compiler/forms-per-eval-default for more information.
      * :omit-compile-ctx - When false Clara, in Clojure, retains additional information to improve error messages during
        session deserialization at the cost of additional memory use.
        By default this information is retained until the session is initially compiled and then will be discarded. This
        information might prove useful for debugging compilation errors within the rulebase, eg. rulebase serialization
        (ie. via Clara's durability support).
        Defaults to true, see clara.rules.compiler/omit-compile-ctx-default for more information.

      This is not supported in ClojureScript, since it requires eval to dynamically build a session. ClojureScript
      users must use pre-defined rule sessions using defsession."
     [& args]
     (if (and (seq args) (not (keyword? (first args))))
       `(com/mk-session ~(vec args)) ; At least one namespace given, so use it.
       `(com/mk-session (concat [(ns-name *ns*)] ~(vec args)))))) ; No namespace given, so use the current one.

#?(:clj
   (defmacro defsession
     "Creates a sesson given a list of sources and keyword-style options, which are typically
     Clojure namespaces.

  NB: This is intended for use in CLJS, where the compilation model is more restrictive in that all
  session compilation must occur at compile-time within the CLJ environment. It is not recommended
  to use `defsession` when targetting CLJ. Instead use `mk-session`.

    Typical usage would be like this, with a session defined as a var:

    (defsession my-session 'example.namespace)

    That var contains an immutable session that then can be used as a starting point to create
  sessions with caller-provided data. Since the session itself is immutable, it can be safely used
  from multiple threads and will not be modified by callers. So a user might grab it, insert facts,
  and otherwise use it as follows:

    (-> my-session
       (insert (->Temperature 23))
       (fire-rules))
  "
     [name & sources-and-options]
     (let [sources-and-options (vec sources-and-options)]
       (if (platform/compiling-cljs?)
         ;; CLJS needs to compile the entire session at compile-time. This will be more restrictive
         ;; than CLJ.
         (let [s (com/mk-session sources-and-options)]
           `(def ~name ~s))
         ;; CLJ waits until runtime.
         `(def ~name (com/mk-session '~sources-and-options))))))

#?(:clj
  (defmacro defrule
    "Defines a rule and stores it in the given var. For instance, a simple rule would look like this:

    (defrule hvac-approval
      \"HVAC repairs need the appropriate paperwork, so insert
        a validation error if approval is not present.\"
      [WorkOrder (= type :hvac)]
      [:not [ApprovalForm (= formname \"27B-6\")]]
      =>
      (insert! (->ValidationError
                :approval
                \"HVAC repairs must include a 27B-6 form.\")))

See the [rule authoring documentation](http://www.clara-rules.org/docs/rules/) for details."
    [name & body]
    (let [doc (if (string? (first body)) (first body) nil)
          def-name (vary-meta name assoc :rule true :doc doc)
          rule (dsl/build-rule name body (meta &form))]
      (when (platform/compiling-cljs?)
        (def def-name rule))
      `(def ~def-name ~rule))))

#?(:clj
  (defmacro defquery
    "Defines a query and stored it in the given var. For instance, a simple query that accepts no
parameters would look like this:

    (defquery check-job
      \"Checks the job for validation errors.\"
      []
      [?issue <- ValidationError])

See the [query authoring documentation](http://www.clara-rules.org/docs/queries/) for details."
    [name & body]
    (let [doc (if (string? (first body)) (first body) nil)
          def-name (vary-meta name assoc :query true :doc doc)
          binding (if doc (second body) (first body))
          definition (if doc (drop 2 body) (rest body))
          query (dsl/build-query name body (meta &form))]
      (when (platform/compiling-cljs?)
        (def def-name query))
      `(def ~def-name ~query))))

#?(:clj
   (defmacro clear-ns-productions!
     "Ensures that any rule/query definitions which have been cached will be cleared from the associated namespace.
      Rule and query definitions can be cached such that if their definitions are not explicitly overwritten with the same
      name (i.e. deleted or renamed), the stale definitions can be loaded into a session using that namespace on
      reload via the REPL or mechanism such as figwheel. Place (clear-ns-productions!) at the top of any namespace
      defining rules/queries to ensure the cache is cleared properly."
     []
     (let [production-syms (->> (ns-interns *ns*)
                                (filter (comp var? second))
                                (filter (comp (some-fn :rule :query :production-seq) meta second)) ; Filter down to rules, queries, and seqs of both.
                                (map first)               ; Take the symbols for the rule/query vars
                                )]
       (doseq [psym production-syms]
         (ns-unmap *ns* psym)))))
