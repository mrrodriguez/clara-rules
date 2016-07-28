(ns clara.rules.durability.print-method
  (:require [clara.rules.durability :as d]
            [clara.rules.memory :as mem]
            [clara.rules.engine :as eng]
            [clara.rules.compiler :as com]
            [clojure.java.io :as jio]
            [clojure.main :as cm])
  (:import [clara.rules.durability
            MemIdx]
           [clara.rules.memory
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
           [org.fressian
            StreamingWriter
            Writer
            Reader
            FressianWriter
            FressianReader]
           [org.fressian.handlers
            WriteHandler
            ReadHandler]
           [java.util
            HashMap
            IdentityHashMap]))

(defmacro defprint-method [dispatch-val args & body]
  (let [args (if (and (vector? args) (= 2 (count args)))
               [(with-meta (first args) {:tag dispatch-val})
                (with-meta (second args) {:tag 'java.io.Writer})]
               (throw (ex-info "Invalid arglist given"
                               {:args args})))]
    `(let [dispatch-val# ~dispatch-val
           orig-impl# (get-method print-method dispatch-val#)]
       (defmethod print-method dispatch-val# ~args
         (if (or (not orig-impl#)
                 d/*serializing-session*)
           (do
             ~@body)
           (orig-impl# ~@args))))))

(def ^:private print-meta @#'clojure.core/print-meta)
(def ^:private print-map @#'clojure.core/print-map)

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

    (if-let [idx (some-> d/*clj-record-holder* (.get o))]
      (do
        (.write w "#=(clara.rules.durability/clj-record-id->fact ")
        (print-method idx w)
        (.write w ")"))
      
      (do
        (.write w "#=(clara.rules.durability/clj-record-fact-holder-add! ")
        (print-record)
        (.write w ")")
        
        (.put d/*clj-record-holder* o (.size d/*clj-record-holder*))))))

(defn print-method-node
  ([node ^java.io.Writer w]
   (print-method-node nil node nil w))
  ([^String header
    node
    ^String footer
    ^java.io.Writer w]
   (let [node-id (:id node)]
     (if (@d/*node-id->node-cache* node-id)
       (do
         (.write w "#=(clara.rules.durability/node-id->node ")
         (print-method node-id w)
         (.write w ")"))
       (do
         (d/cache-node node)
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

(defprint-method clojure.lang.MapEntry [o ^Writer w]
  (.write w "#=(clara.rules.durability/create-map-entry ")
  (print-method (key o) w)
  (.write w " ")
  (print-method (val o) w)
  (.write w ")"))

(defprint-method clojure.lang.IRecord [o w]
  (print-method-record o w))

(defprint-method Class [o w]
  (.write w "#=(resolve ")
  (print-method (symbol (.getName o)) w)
  (.write w ")"))

(defprint-method ProductionNode [o w]
  (print-method-node "#=(clara.rules.durability/add-rhs-fn "
                     (assoc o :rhs nil)
                     ")"
                     w))

(defprint-method QueryNode [o w]
  (print-method-node o w))

(defprint-method AlphaNode [o w]
  (.write w "#=(clara.rules.durability/add-alpha-fn ")
  ;; AlphaNode's do not have an :id
  (print-method-record (assoc o :activation nil) w)
  (.write w ")"))

(defprint-method RootJoinNode [o w]
  (print-method-node o w))

(defprint-method HashJoinNode [o w]
  (print-method-node o w))

(defprint-method ExpressionJoinNode [o w]
  (write-join-filter-node o w))

(defprint-method NegationNode [o w]
  (print-method-node o w))

(defprint-method NegationWithJoinFilterNode [o w]
  (write-join-filter-node o w))

(defprint-method TestNode [o w]
  (print-method-node "#=(clara.rules.durability/add-test-fn "
                  (assoc o :test nil)
                  ")"
                  w))

(defprint-method AccumulateNode [o w]
  (print-method-node "#=(clara.rules.durability/add-accumulator "
                  (assoc o :accumulator nil)
                  ")"
                  w))

(defprint-method AccumulateWithJoinFilterNode [o w]
  (print-method-node "#=(clara.rules.durability/add-accumulator #=(clara.rules.durability/add-join-filter-fn "
                  (assoc o :accumulator nil :join-filter-fn nil)
                  "))"
                  w))

(defprint-method RuleOrderedActivation [o w]
  (.write w "#=(clara.rules.memory/->RuleOrderedActivation ")
  (print-method (.-node-id o) w)
  (print-method (.-token o) w)
  (print-method (.-activation o) w)
  (print-method (.-rule-load-order o) w)
  (.write w ")"))

(defn- print-method-sorted [create-fn-str seq-fn ^clojure.lang.Sorted s ^java.io.Writer w]
  (let [cname (d/sorted-comparator-name s)]
    
    (.write w (str "#=(" create-fn-str " "))
    (when cname
      (.write w "#=(deref #=(var ")
      (print-method cname w)
      (.write w ") )"))
    ;; The seq of a sorted coll may returns entry types that do not have a valid print-method
    ;; representation.  To workaround that, replace them all with 2 item vectors instead.
    (print-method (seq-fn s) w)
    (.write w ")")))

;;;; TODO use seq->sorted-set/map for these
(defprint-method clojure.lang.PersistentTreeSet [o w]
  (print-method-sorted "clojure.lang.PersistentTreeSet/create" seq o w))

(defprint-method clojure.lang.PersistentTreeMap [o w]
  (print-method-sorted "clojure.lang.PersistentTreeMap/create" #(sequence cat %) o w))

(defprint-method MemIdx [o w]
  (.write w "#=(clara.rules.durability/find-mem-idx ")
  (print-method (:idx o) w)
  (.write w ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Session serializer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord PrintMethodSessionSerializer [in-stream out-stream]
  d/ISessionSerializer
  (serialize [_ session opts]
    (let [{:keys [rulebase memory]} (eng/components session)
          record-holder (IdentityHashMap.)

          do-serialize (fn [print-method-sources]
                         (with-open [wtr (jio/writer out-stream)]
                           (binding [d/*node-id->node-cache* (volatile! {})
                                     d/*clj-record-holder* record-holder
                                     d/*serializing-session* true
                                     *print-meta* true
                                     *print-readably* true
                                     *out* wtr]
                             (doseq [prd print-method-sources] (pr prd))
                             (flush))))]

      ;; In this case there is nothing to do with memory, so just serialize immediately.
      (if (:rulebase-only? opts)
        (do-serialize [rulebase])

        ;; Otherwise memory needs to have facts extracted to return.
        (let [{:keys [memory indexed-facts]} (d/session-memory-state memory)
              print-method-sources (cond
                                     (:with-rulebase? opts) [rulebase memory]
                                     :else [memory])]

          (do-serialize print-method-sources)
          
          ;; Return the facts needing to be serialized still.
          indexed-facts))))

  (deserialize [_ mem-facts opts]
    (with-open [rdr (clojure.lang.LineNumberingPushbackReader. (jio/reader in-stream))]
      (let [{:keys [rulebase-only? base-rulebase]} opts
            record-holder (IdentityHashMap.)
            ;; The rulebase should either be given from the base-session or found in
            ;; the restored session-state.
            rulebase (or (and (not rulebase-only?) base-rulebase)
                         (binding [d/*node-id->node-cache* (volatile! {})
                                   d/*clj-record-holder* record-holder
                                   d/*compile-expr-fn* (memoize (fn [id expr] (com/try-eval expr)))]
                           (read rdr)))]

        (if rulebase-only?
          rulebase
          (d/assemble-restored-session rulebase
                                       (binding [d/*clj-record-holder* record-holder
                                                 d/*mem-facts* mem-facts]
                                         (read rdr))
                                       opts))))))

(defn create-session-serializer
  ([in+out-stream]
   (create-session-serializer in+out-stream in+out-stream))
  ([in-stream out-stream]
   (->PrintMethodSessionSerializer in-stream out-stream)))
