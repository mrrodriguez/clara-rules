(def cljs-compile-target "resources/public/js/compiled")

(defproject com.cerner/clara-rules "0.21.0-SNAPSHOT"
  :description "Clara Rules Engine"
  :url "https://github.com/cerner/clara-rules"
  :license {:name "Apache License Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}
  :plugins [[lein-codox "0.10.7"]
            [lein-javadoc "0.3.0"]
            [lein-shell "0.5.0"]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [prismatic/schema "1.1.12"]]
  :aliases {"clean" ["do"
                     ["with-profile" "clean-js" "clean"]
                     ["clean"]]
            "build-dev-assets" ["do"
                                ["shell" "npm" "install"]]
            "fig" ["trampoline" "run" "-m" "figwheel.main"]
            "fig-repl" ["do"
                        ["build-dev-assets"]
                        ["with-profiles" "dev-cmd-line" ["fig" "-b" "dev" "-r"]]]}
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure" "src/test/cljsenv"]
  :java-source-paths ["src/main/java"]
  :profiles {:clean-js {:clean-targets ^{:protect false} [~cljs-compile-target]}
             :cljs-common {
                           ;; Only needed for cljs 1.9 compat - fixed in newer versions, but we want
                           ;; to test with an older baseline in this project for now.
                           :dependencies [[javax.xml.bind/jaxb-api "2.4.0-b180830.0359"]
                                          [com.bhauman/figwheel-main "0.2.3"]]
                           :test-commands {"phantom-simple" ["phantomjs"
                                                             "src/test/js/runner.js"
                                                             "src/test/html/simple.html"]

                                           "phantom-advanced" ["phantomjs"
                                                               "src/test/js/runner.js"
                                                               "src/test/html/advanced.html"]}}
             :cljs-dev [:cljs-common
                        {:source-paths ["src/test/clojure"]
                         :resource-paths ["resources" "dev-resources"]}]
             :dev [:cljs-dev
                   {:dependencies [[org.clojure/math.combinatorics "0.1.3"]
                                   [org.clojure/data.fressian "0.2.1"]
                                   [vvvvalvalval/scope-capture "0.2.0"]]
                    :source-paths ["dev"]
                    :resource-paths ["dev-resources"]
                    :java-source-paths ["src/test/java"]
                    :global-vars {*warn-on-reflection* true}}]
             :dev-cmd-line [:dev
                            {:dependencies [[com.bhauman/rebel-readline-cljs "0.1.4"]]}]
             :provided {:dependencies [[org.clojure/clojurescript "1.9.946"]]}
             :recent-clj {:dependencies [[org.clojure/clojure "1.10.1"]
                                         [org.clojure/clojurescript "1.10.520"]]}}

  :codox {:namespaces [clara.rules clara.rules.dsl clara.rules.accumulators
                       clara.rules.listener clara.rules.durability
                       clara.tools.inspect clara.tools.tracing
                       clara.tools.fact-graph]
          :metadata {:doc/format :markdown}}
  :javadoc-opts {:package-names "clara.rules"}
  ;; Factoring out the duplication of this test selector function causes an error,
  ;; perhaps because Leiningen is using this as uneval'ed code.
  ;; For now just duplicate the line.
  :test-selectors {:default
                   (complement (fn [x]
                                 (let [blacklisted-packages #{"generative" "performance"}
                                       patterns (into []
                                                      (comp
                                                       (map #(str "^clara\\." % ".*"))
                                                       (interpose "|"))
                                                      blacklisted-packages)]
                                   (some->> x :ns ns-name str (re-matches (re-pattern (apply str patterns)))))))
                   :generative
                   (fn [x] (some->> x :ns ns-name str (re-matches #"^clara\.generative.*")))
                   :performance
                   (fn [x] (some->> x :ns ns-name str (re-matches #"^clara\.performance.*")))}
  :scm {:name "git"
        :url "https://github.com/cerner/clara-rules"}
  :pom-addition [:developers [:developer
                              [:id "rbrush"]
                              [:name "Ryan Brush"]
                              [:url "http://www.clara-rules.org"]]]
  :deploy-repositories [["snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/"
                                      :creds :gpg}]])
