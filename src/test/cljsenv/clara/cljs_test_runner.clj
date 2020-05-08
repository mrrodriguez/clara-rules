(ns clara.cljs-test-runner
  "Test runner config for CLJS tests.
   Inspired byhttps://betweentwoparens.com/clojurescript-test-setup"
  (:require [clojure.java.shell :as sh]))

(defn run-js-test-environment
  [{:keys [output-to open-url] :as args}]
  (let [js-env "src/test/js/test_environment.js"
        result (sh/sh "node" js-env output-to open-url)]
    (if (zero? (:exit result))
      result
      (do
        (println result)
        (System/exit 0)))))
