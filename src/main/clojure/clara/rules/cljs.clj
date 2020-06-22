(ns clara.rules.cljs
  (:require [clara.rules.platform :as p]
            [cljs.env :as env]))

(defn get-namespace-info
  "Get metadata about the given namespace."
  [namespace]
  (when-let [n (and (p/compiling-cljs?) (find-ns 'cljs.env))]
    (when-let [v (ns-resolve n '*compiler*)]
      (get-in @@v [:cljs.analyzer/namespaces namespace]))))

(defn cljs-ns
  "Returns the ClojureScript namespace being compiled during Clojurescript compilation."
  []
  (when (p/compiling-cljs?)
    (-> 'cljs.analyzer (find-ns) (ns-resolve '*cljs-ns*) deref)))


;;; Clear productions stored in cljs.env/*compiler* for current namespace.
;;; Only exists for its side-effect, hence returns nil.
(defmacro clear-ns-productions!
  []
  (swap! env/*compiler* assoc-in [:clara.rules/productions (cljs-ns)] {})
  nil)

;; Store production in cljs.env/*compiler* under :clara.rules/productions seq?
(defn add-production [name production]
  (swap! env/*compiler*
         assoc-in
         [:clara.rules/productions (cljs-ns) name]
         production))

(defn- get-productions-from-namespace
  "Returns a map of names to productions in the given namespace."
  [namespace]
  ;; TODO: remove need for ugly eval by changing our quoting strategy.
  (let [productions (get-in @env/*compiler* [:clara.rules/productions namespace])]
    (map eval (vals productions))))

(defn resolve-cljs-sym
  "Resolves a ClojureScript symbol in the given namespace."
  [ns-sym sym]
  (let [ns-info (get-namespace-info ns-sym)]
    (if (namespace sym)

      ;; Symbol qualified by a namespace, so look it up in the requires info.
      (if-let [source-ns (get-in ns-info [:requires (symbol (namespace sym))])]
        (symbol (name source-ns) (name sym))
        ;; Not in the requires block, so assume the qualified name is a refers and simply return the symbol.
        sym)

      ;; Symbol is unqualified, so check in the uses block.
      (if-let [source-ns (get-in ns-info [:uses sym])]
        (symbol (name source-ns) (name sym))

        ;; Symbol not found in eiher block, so attempt to retrieve it from
        ;; the current namespace.
        (if (get-in (get-namespace-info ns-sym) [:defs sym])
          (symbol (name ns-sym) (name sym))
          nil)))))
