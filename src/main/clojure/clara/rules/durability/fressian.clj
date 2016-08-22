(ns clara.rules.durability.fressian
  (:require [clara.rules.durability :as d]
            [clara.rules.memory :as mem]
            [clara.rules.engine :as eng]
            [clara.rules.compiler :as com]
            [clojure.data.fressian :as fres]
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
            ArrayList
            IdentityHashMap]))

;;; TODO cache these based on class?  Will just speed up serialization.
(defn record-map-constructor-name
  "Return the map constructor for a record"
  [rec]
  (let [class-name (-> rec class .getName)
        idx (.lastIndexOf class-name (int \.))
        ns-nom (.substring class-name 0 idx)
        nom (.substring class-name (inc idx))]
    (symbol (str (cm/demunge ns-nom)
                 "/map->"
                 (cm/demunge nom)))))

(defn write-map [^Writer w m]
  (.writeTag w "map" 1)
  (.beginClosedList ^StreamingWriter w)
  (reduce-kv
   (fn [^Writer w k v]
     (.writeObject w k true)
     (.writeObject w v))
   w
   m)
  (.endList ^StreamingWriter w))

(defn write-with-meta
  ([w tag o]
   (write-with-meta w tag o (fn [^Writer w o] (.writeList w o))))
  ([^Writer w tag o write-fn]
   (let [m (meta o)]
     (do
       (.writeTag w tag 2)
       (write-fn w o)
       (if m
         (.writeObject w m)
         (.writeNull w))))))

(defn read-meta [^Reader rdr]
  (some->> rdr
           .readObject
           (into {})))

(defn read-with-meta [^Reader rdr build-fn]
  (let [o (build-fn (.readObject rdr))
        m (read-meta rdr)]
    (cond-> o
      m (with-meta m))))

(defn write-record
  [^Writer w tag rec]
  (let [m (meta rec)]
    (.writeTag w tag 3)
    (.writeObject w (record-map-constructor-name rec) true)
    (write-map w rec)
    (if m
      (.writeObject w m)
      (.writeNull w))))

(defn read-record
  ([^Reader rdr]
   (read-record rdr nil))
  ([^Reader rdr add-fn]
   (let [builder (-> (.readObject rdr) resolve deref)
         build-map (.readObject rdr)
         m (read-meta rdr)]
     (cond-> (builder build-map)
       m (with-meta m)
       add-fn add-fn))))

(defn- create-cached-node-handler
  ([clazz
    tag
    tag-for-cached]
   {:class clazz
    :writer (reify WriteHandler
              (write [_ w o]
                (let [node-id (:id o)]
                  (if (@d/*node-id->node-cache* node-id)
                    (do
                      (.writeTag w tag-for-cached 1)
                      (.writeInt w node-id))
                    (do
                      (d/cache-node o)
                      (write-record w tag o))))))
    :readers {tag-for-cached
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (d/node-id->node (.readObject rdr))))
              tag
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (-> rdr
                      read-record
                      d/cache-node)))}})
  ([clazz
    tag
    tag-for-cached
    remove-node-expr-fn
    add-node-expr-fn]
   {:class clazz
    :writer (reify WriteHandler
              (write [_ w o]
                (let [node-id (:id o)]
                  (if (@d/*node-id->node-cache* node-id)
                    (do
                      (.writeTag w tag-for-cached 1)
                      (.writeInt w node-id))
                    (do
                      (d/cache-node o)
                      (write-record w tag (remove-node-expr-fn o)))))))
    :readers {tag-for-cached
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (d/node-id->node (.readObject rdr))))
              tag
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (-> rdr
                      (read-record add-node-expr-fn)
                      d/cache-node)))}}))

(def handlers
  {"java/class"
   {:class Class
    :writer (reify WriteHandler
              (write [_ w c]
                (.writeTag w "java/class" 1)
                (.writeObject w (symbol (.getName ^Class c)) true)))
    :readers {"java/class"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (resolve (.readObject rdr))))}}

   "clj/set"
   {:class clojure.lang.APersistentSet
    :writer (reify WriteHandler
              (write [_ w o]
                (write-with-meta w "clj/set" o)))
    :readers {"clj/set"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (read-with-meta rdr set)))}}

   "clj/vector"
   {:class clojure.lang.APersistentVector
    :writer (reify WriteHandler
              (write [_ w o]
                (write-with-meta w "clj/vector" o)))
    :readers {"clj/vector"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (read-with-meta rdr vec)))}}

   "clj/list"
   {:class clojure.lang.PersistentList
    :writer (reify WriteHandler
              (write [_ w o]
                (write-with-meta w "clj/list" o)))
    :readers {"clj/list"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (read-with-meta rdr #(apply list %))))}}

   "clj/aseq"
   {:class clojure.lang.ASeq
    :writer (reify WriteHandler
              (write [_ w o]
                (write-with-meta w "clj/aseq" o)))
    :readers {"clj/aseq"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (read-with-meta rdr sequence)))}}

   "clj/lazyseq"
   {:class clojure.lang.LazySeq
    :writer (reify WriteHandler
              (write [_ w o]
                (write-with-meta w "clj/lazyseq" o)))
    :readers {"clj/lazyseq"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (read-with-meta rdr sequence)))}}

   "clj/map"
   {:class clojure.lang.APersistentMap
    :writer (reify WriteHandler
              (write [_ w o]
                (write-with-meta w "clj/map" o write-map)))
    :readers {"clj/map"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (read-with-meta rdr #(into {} %))))}}
   
   "clj/treeset"
   {:class clojure.lang.PersistentTreeSet
    :writer (reify WriteHandler
              (write [_ w o]
                (let [cname (d/sorted-comparator-name o)]
                  (.writeTag w "clj/treeset" 2)
                  (if cname
                    (.writeObject w cname true)
                    (.writeNull w))
                  (.writeList w o))))
    :readers {"clj/treeset"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (let [c (some-> rdr .readObject resolve deref)]
                    (d/seq->sorted-set (.readObject rdr) c))))}}

   "clj/treemap"
   {:class clojure.lang.PersistentTreeMap
    :writer (reify WriteHandler
              (write [_ w o]
                (let [cname (d/sorted-comparator-name o)]
                  (.writeTag w "clj/treemap" 2)
                  (if cname
                    (.writeObject w cname true)
                    (.writeNull w))
                  (write-map w o))))
    :readers {"clj/treemap"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (let [c (some-> rdr .readObject resolve deref)]
                    (d/seq->sorted-map (.readObject rdr) c))))}}

   "clj/mapentry"
   {:class clojure.lang.MapEntry
    :writer (reify WriteHandler
              (write [_ w o]
                (.writeTag w "clj/mapentry" 2)
                (.writeObject w (key o) true)
                (.writeObject w (val o))))
    :readers {"clj/mapentry"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (d/create-map-entry (.readObject rdr)
                                      (.readObject rdr))))}}

   ;; Have to redefine both Symbol and IRecord to support metadata as well
   ;; as identity-based caching for the IRecord case.

   "clj/sym"
   {:class clojure.lang.Symbol
    :writer (reify WriteHandler
              (write [_ w o]
                ;; Mostly copied from private fres/write-named, except the metadata part.
                (.writeTag w "clj/sym" 3)
                (.writeObject w (namespace o) true)
                (.writeObject w (name o) true)
                (if-let [m (meta o)]
                  (.writeObject w m)
                  (.writeNull w))))
    :readers {"clj/sym"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (let [s (symbol (.readObject rdr) (.readObject rdr))
                        m (read-meta rdr)]
                    (cond-> s
                      m (with-meta m)))))}}

   "clj/record"
   {:class clojure.lang.IRecord
    ;; Write a record a single time per object reference to that record.  The record is then "cached"
    ;; with the IdentityHashMap `d/*clj-record-holder*`.  If another reference to this record instance
    ;; is encountered later, only the "index" of the record in the map will be written.
    :writer (reify WriteHandler
              (write [_ w rec]
                (if-let [idx (d/clj-record-fact->idx rec)]
                  (do
                    (.writeTag w "clj/recordidx" 1)
                    (.writeInt w idx))
                  (do
                    (write-record w "clj/record" rec)
                    (d/clj-record-holder-add-fact-idx! rec)))))
    ;; When reading the first time a reference to a record instance is found, the entire record will
    ;; need to be constructed.  It is then put into indexed cache.  If more references to this record
    ;; instance are encountered later, they will be in the form of a numeric index into this cache.
    ;; This is guaranteed by the semantics of the corresponding WriteHandler.
    :readers {"clj/recordidx"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (d/clj-record-idx->fact (.readInt rdr))))
              "clj/record"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (-> rdr
                      read-record
                      d/clj-record-holder-add-fact!)))}}

   "clara/productionnode"
   (create-cached-node-handler ProductionNode
                               "clara/productionnode"
                               "clara/productionnodeid"
                               #(assoc % :rhs nil)
                               d/add-rhs-fn)

   "clara/querynode"
   (create-cached-node-handler QueryNode
                               "clara/querynode"
                               "clara/querynodeid")

   "clara/alphanode"
   {:class AlphaNode
    ;; The writer and reader here work similar to the IRecord implementation.  The only
    ;; difference is that the record needs to be written with out the compiled clj
    ;; function on it.  This is due to clj functions not having any convenient format
    ;; for serialization.  The function is restored by re-eval'ing the function based on
    ;; its originating code form at read-time.
    :writer (reify WriteHandler
              (write [_ w o]
                (if-let [idx (d/clj-record-fact->idx o)]
                  (do
                    (.writeTag w "clara/alphanodeid" 1)
                    (.writeInt w idx))
                  (do
                    (write-record w "clara/alphanode" (assoc o :activation nil))
                    (d/clj-record-holder-add-fact-idx! o)))))
    :readers {"clara/alphanodeid"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (d/clj-record-idx->fact (.readObject rdr))))
              "clara/alphanode"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (-> rdr
                      (read-record d/add-alpha-fn)
                      d/clj-record-holder-add-fact!)))}}

   "clara/rootjoinnode"
   (create-cached-node-handler RootJoinNode
                               "clara/rootjoinnode"
                               "clara/rootjoinnodeid")

   "clara/hashjoinnode"
   (create-cached-node-handler HashJoinNode
                               "clara/hashjoinnode"
                               "clara/hashjoinnodeid")

   "clara/exprjoinnode"
   (create-cached-node-handler ExpressionJoinNode
                               "clara/exprjoinnode"
                               "clara/exprjoinnodeid"
                               #(assoc % :join-filter-fn nil)
                               d/add-join-filter-fn)

   "clara/negationnode"
   (create-cached-node-handler NegationNode
                               "clara/negationnode"
                               "clara/negationnodeid")

   "clara/negationwjoinnode"
   (create-cached-node-handler NegationWithJoinFilterNode
                               "clara/negationwjoinnode"
                               "clara/negationwjoinnodeid"
                               #(assoc % :join-filter-fn nil)
                               d/add-join-filter-fn)

   "clara/testnode"
   (create-cached-node-handler TestNode
                               "clara/testnode"
                               "clara/testnodeid"
                               #(assoc % :test nil)
                               d/add-test-fn)

   "clara/accumnode"
   (create-cached-node-handler AccumulateNode
                               "clara/accumnode"
                               "clara/accumnodeid"
                               #(assoc % :accumulator nil)
                               d/add-accumulator)

   "clara/accumwjoinnode"
   (create-cached-node-handler AccumulateWithJoinFilterNode
                               "clara/accumwjoinnode"
                               "clara/accumwjoinnodeid"
                               #(assoc % :accumulator nil :join-filter-fn nil)
                               (comp d/add-accumulator d/add-join-filter-fn))

   "clara/ruleorderactivation"
   {:class RuleOrderedActivation
    :writer (reify WriteHandler
              (write [_ w c]
                (.writeTag w "clara/ruleorderactivation" 4)
                (.writeObject w (.-node-id ^RuleOrderedActivation c) true)
                (.writeObject w (.-token ^RuleOrderedActivation c))
                (.writeObject w (.-activation ^RuleOrderedActivation c))
                (.writeInt w (.-rule-load-order ^RuleOrderedActivation c))))
    :readers {"clara/ruleorderactivation"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (mem/->RuleOrderedActivation (.readObject rdr)
                                               (.readObject rdr)
                                               (.readObject rdr)
                                               (.readObject rdr))))}}

   "clara/memidx"
   {:class MemIdx
    :writer (reify WriteHandler
              (write [_ w c]
                (.writeTag w "clara/memidx" 1)
                (.writeInt w (:idx c))))
    :readers {"clara/memidx"
              (reify ReadHandler
                (read [_ rdr tag component-count]
                  (d/find-mem-idx (.readObject rdr))))}}})

(def write-handlers
  (into fres/clojure-write-handlers
        (map (fn [[tag {clazz :class wtr :writer}]]
               [clazz {tag wtr}]))
        handlers))

(def write-handler-lookup
  (-> write-handlers
      fres/associative-lookup
      fres/inheritance-lookup))

(def read-handlers
    (->> handlers
       vals
       (into fres/clojure-read-handlers
             (mapcat :readers))))

(def read-handler-lookup
  (fres/associative-lookup read-handlers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Session serializer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord FressianSessionSerializer [in-stream out-stream]
  d/ISessionSerializer
  (serialize [_ session opts]
    (let [{:keys [rulebase memory]} (eng/components session)
          record-holder (IdentityHashMap.)
          do-serialize
          (fn [sources]
            (with-open [^FressianWriter wtr
                        (fres/create-writer out-stream :handlers write-handler-lookup)]
              (binding [d/*node-id->node-cache* (volatile! {})
                        d/*clj-record-holder* record-holder]
                (doseq [s sources] (fres/write-object wtr s)))))]

      ;; In this case there is nothing to do with memory, so just serialize immediately.
      (if (:rulebase-only? opts)
        (do-serialize [rulebase])

        ;; Otherwise memory needs to have facts extracted to return.
        (let [{:keys [memory indexed-facts]} (d/indexed-session-memory-state memory)
              sources (cond
                        (:with-rulebase? opts) [rulebase memory]
                        :else [memory])]

          (do-serialize sources)
          
          ;; Return the facts needing to be serialized still.
          indexed-facts))))

  (deserialize [_ mem-facts opts]
    (with-open [^FressianReader rdr (fres/create-reader in-stream :handlers read-handler-lookup)]
      (let [{:keys [rulebase-only? base-rulebase]} opts

            record-holder (ArrayList.)
            ;; The rulebase should either be given from the base-session or found in
            ;; the restored session-state.
            rulebase (or (and (not rulebase-only?) base-rulebase)
                         (binding [d/*node-id->node-cache* (volatile! {})
                                   d/*clj-record-holder* record-holder
                                   d/*compile-expr-fn* (memoize (fn [id expr] (com/try-eval expr)))]
                           (fres/read-object rdr)))]

        (if rulebase-only?
          rulebase
          (d/assemble-restored-session rulebase
                                       (binding [d/*clj-record-holder* record-holder
                                                 d/*mem-facts* mem-facts]
                                         (fres/read-object rdr))
                                       opts))))))

(defn create-session-serializer
  ([in+out-stream]
   (create-session-serializer in+out-stream in+out-stream))
  ([in-stream out-stream]
   (->FressianSessionSerializer in-stream out-stream)))
