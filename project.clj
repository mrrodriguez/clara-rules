(defproject com.cerner/clara-rules "0.18.0-SNAPSHOT"
  :description "Clara Rules Engine"
  :url "https://github.com/cerner/clara-rules"
  :license {:name "Apache License Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [prismatic/schema "1.1.6"]]
  :plugins [[lein-codox "0.10.3" :exclusions [org.clojure/clojure
                                              org.clojure/clojurescript]]
            [lein-javadoc "0.3.0" :exclusions [org.clojure/clojure
                                               org.clojure/clojurescript]]
            [lein-cljsbuild "1.1.7" :exclusions [org.clojure/clojure
                                                 org.clojure/clojurescript]]
            [lein-figwheel "0.5.15" :exclusions [org.clojure/clojure
                                                 org.clojure/clojurescript]]]
  :codox {:namespaces [clara.rules clara.rules.dsl clara.rules.accumulators
                       clara.rules.listener clara.rules.durability
                       clara.tools.inspect clara.tools.tracing
                       clara.tools.fact-graph]
          :metadata {:doc/format :markdown}}
  :javadoc-opts {:package-names "clara.rules"}
  :source-paths ["src/main/clojure"]
  :resource-paths []
  :test-paths ["src/test/clojure" "src/test/common"]
  :java-source-paths ["src/main/java"]
  :javac-options ["-target" "1.6" "-source" "1.6"]
  :clean-targets ^{:protect false} ["resources/public/js" "target"]
  :aliases {"test-cljs" ["do" "clean"
                         ["with-profile" "provided, cljs-test"
                          ["cljsbuild" "test"]]]}
  :profiles {:provided {:dependencies [[org.clojure/clojurescript "1.7.170"]]}
             :recent-clj {:dependencies [^:replace [org.clojure/clojure "1.9.0"]
                                         ^:replace [org.clojure/clojurescript "1.9.946"]]}
             ;; figwheel-sidecar needs a more recent cljs to work correctly in a popular repl client
             ;; like the emacs cider repl.
             :cljs-dev [:recent-clj
                        {:dependencies [[figwheel-sidecar "0.5.15"]
                                        [com.cemerick/piggieback "0.2.2"]
                                        [binaryage/devtools "0.9.0"]]
                         :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                         :cljsbuild
                         {:builds {:app
                                   ;; Simple mode compilation for tests.
                                   {:source-paths ["src/test/clojurescript" "src/test/common"]
                                    :figwheel {}
                                    :compiler {:main "clara.test"
                                               :output-to "resources/public/js/simple.js"
                                               :output-dir "resources/public/js/out"
                                               :asset-path "js/out"
                                               :preloads [devtools.preload]
                                               :source-map true
                                               :source-map-timestamp true
                                               :pretty-print true
                                               :closure-defines {goog.DEBUG true}
                                               :optimizations :none}}}}}]
             ;; NOTE: for smoother figwheel integration, :dev needs to include :cljs-dev for now.
             ;; This means that :recent-clj is also loaded in :dev. However, tests can still be
             ;; ran against older versions of clj/cljs that Clara supports.
             :dev [:cljs-dev
                   {:dependencies [[org.clojure/math.combinatorics "0.1.3"]
                                   [org.clojure/data.fressian "0.2.1"]]}]
             :cljs-test {:cljsbuild
                         {:builds {:simple
                                   {:source-paths ["src/test/clojurescript" "src/test/common"]
                                    :compiler {:output-to "target/js/simple.js"
                                               :pretty-print false
                                               :optimizations :whitespace}}

                                   ;; Advanced mode compilation for tests.
                                   :advanced
                                   {:source-paths ["src/test/clojurescript" "src/test/common"]
                                    :compiler {:output-to "target/js/advanced.js"
                                               :optimizations :advanced}}}
                          :test-commands {"phantom-simple" ["phantomjs"
                                                            "src/test/js/runner.js"
                                                            "src/test/html/simple.html"]

                                          "phantom-advanced" ["phantomjs"
                                                              "src/test/js/runner.js"
                                                              "src/test/html/advanced.html"]}}}}
  
  ;; Factoring out the duplication of this test selector function causes an error,
  ;; perhaps because Leiningen is using this as uneval'ed code.
  ;; For now just duplicate the line.
  :test-selectors {:default (complement (fn [x]
                                          (some->> x :ns ns-name str (re-matches #"^clara\.generative.*"))))
                   :generative (fn [x] (some->> x :ns ns-name str (re-matches #"^clara\.generative.*")))}
  
  :scm {:name "git"
        :url "https://github.com/cerner/clara-rules"}
  :pom-addition [:developers [:developer
                              [:id "rbrush"]
                              [:name "Ryan Brush"]
                              [:url "http://www.clara-rules.org"]]]
  :deploy-repositories [["snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/"
                                      :creds :gpg}]])
