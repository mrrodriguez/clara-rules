(defproject com.cerner/clara-rules "0.20.0-SNAPSHOT"
  :description "Clara Rules Engine"
  :url "https://github.com/cerner/clara-rules"
  :license {:name "Apache License Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}
  :scm {:name "git"
        :url "https://github.com/cerner/clara-rules"}
  :plugins [[lein-codox "0.10.3" :exclusions [org.clojure/clojure
                                              org.clojure/clojurescript]]
            [lein-javadoc "0.3.0" :exclusions [org.clojure/clojure
                                               org.clojure/clojurescript]]
            [lein-cljsbuild "1.1.7" :exclusions [org.clojure/clojure
                                                 org.clojure/clojurescript]]
            [lein-figwheel "0.5.18" :exclusions [org.clojure/clojure
                                                 org.clojure/clojurescript]]
            [lein-shell "0.5.0"]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [prismatic/schema "1.1.6"]]
  :aliases {"cljs-test" ["cljsbuild" "test"]
            "test-all" ["do"
                        ["shell" "echo" "**** Running Clojure tests ****"]
                        ["test"]
                        ["shell" "echo" "**** Running ClojureScript tests ****"]
                        ["cljs-test"]]
            "test-all-clj7" ["do"
                             ["shell" "echo" "Running CLJ/CLJS 7 tests..."]
                             ["with-profile" "+clj7" "test-all"]]
            "test-all-clj10" ["do"
                              ["shell" "echo" "Running CLJ/CLJS 10 tests..."]
                              ["with-profile" "+clj10" "test-all"]]
            "test-all-jdk11" ["with-profile" "+jdk11" "test-all"]}
  :profiles
  {:cljs-common {:dependencies [[org.clojure/clojurescript "1.10.339"]]
                 :source-paths ["src"]
                 :cljsbuild {:test-commands {"phantom-simple" ["phantomjs"
                                                               "src/test/js/runner.js"
                                                               "src/test/html/simple.html"]

                                             "phantom-advanced" ["phantomjs"
                                                                 "src/test/js/runner.js"
                                                                 "src/test/html/advanced.html"]}}
                 :compiler {:npm-deps false}}
   :figwheel-dev {:source-paths ["src/test/clojurescript" "src/test/common"]
                  :dependencies [[figwheel-sidecar "0.5.18"]]
                  :cljsbuild {:builds
                              {:dev-repl {:source-paths ["src/test/clojurescript" "src/test/common"]
                                          :figwheel true
                                          :compiler {:output-to "resources/public/js/simple.js"
                                                     :output-dir "resources/public/js/out"
                                                     :asset-path "js/out"
                                                     :optimizations :none}}

                               :simple {:id "simple"
                                        :source-paths ["src/test/clojurescript" "src/test/common"]
                                        :compiler {:output-to "target/js/simple.js"
                                                   :optimizations :whitespace}}

                               ;; Advanced mode compilation for tests.
                               :advanced {:id "advanced"
                                          :source-paths ["src/test/clojurescript" "src/test/common"]
                                          :compiler {:output-to "target/js/advanced.js"
                                                     :optimizations :advanced}}}}}
   :dev [:cljs-common
         :figwheel-dev
         {:dependencies [[org.clojure/math.combinatorics "0.1.3"]
                         [org.clojure/data.fressian "0.2.1"]]}]
   ;; Min officially tested/supported versions of CLJ/CLJS.
   :clj7 {:dependencies [^:replace [org.clojure/clojure "1.7.0"]
                         ^:replace [org.clojure/clojurescript "1.7.170"]]}
   ;; Using latest (as of Apr 5, 2019) 1.10.x deps for CLJ/CLJS.
   :clj10 {:dependencies [^:replace [org.clojure/clojure "1.10.0"]
                          ^:replace [org.clojure/clojurescript "1.10.520"]]}
   :jdk9 {:jvm-opts ["--add-modules=java.xml.bind"]}
   :jdk11 [:clj10
           {:min-lein-version "2.8.0"}]}

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
  :clean-targets ^{:protect false} ["resources/public/js" "target"]
  :repl-options {;; The large number of ClojureScript tests is causing long compilation times
                 ;; to start the REPL.
                 :timeout 180000}
  ;; Factoring out the duplication of this test selector function causes an error,
  ;; perhaps because Leiningen is using this as uneval'ed code.
  ;; For now just duplicate the line.
  :test-selectors {:default (complement (fn [x]
                                          (let [blacklisted-packages #{"generative" "performance"}
                                                patterns (into []
                                                           (comp
                                                             (map #(str "^clara\\." % ".*"))
                                                             (interpose "|"))
                                                           blacklisted-packages)]
                                            (some->> x :ns ns-name str (re-matches (re-pattern (apply str patterns)))))))
                   :generative (fn [x] (some->> x :ns ns-name str (re-matches #"^clara\.generative.*")))
                   :performance (fn [x] (some->> x :ns ns-name str (re-matches #"^clara\.performance.*")))}
  :pom-addition [:developers [:developer
                              [:id "rbrush"]
                              [:name "Ryan Brush"]
                              [:url "http://www.clara-rules.org"]]]
  :deploy-repositories [["snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/"
                                      :creds :gpg}]])
