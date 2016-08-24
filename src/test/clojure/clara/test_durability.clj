(ns clara.test-durability
  (:require [clara.rules :refer :all]
            [clara.rules.dsl :as dsl]
            [clara.rules.engine :as eng]
            [clara.rules.durability :as d]
            [clara.rules.durability.fressian :as df]
            [clara.durability-rules :as dr]
            [clara.rules.accumulators :as acc]
            [clara.rules.testfacts :refer :all]
            [schema.test :as st]
            [clojure.java.io :as jio]
            [clojure.test :refer :all])
  (:import [clara.rules.testfacts
            Temperature]))

(use-fixtures :once st/validate-schemas)

(defrecord LocalMemorySerializer [holder]
  d/IWorkingMemorySerializer
  (serialize-facts [_ fact-seq]
    (reset! holder fact-seq))
  (deserialize-facts [_]
    @holder))

(def rulebase-data (java.io.File/createTempFile "rulebase" ".dat"))
(def session-data (java.io.File/createTempFile "session" ".dat"))

(defn check-fact
  "Helper for checking facts in durability-test."
  [expected-fact fact]

  ;; Test equality first.  The tests need to be stricter than this, but if this isn't true nothing
  ;; else will be.  This will give the best failure messages first too in the more obvious non-equal
  ;; cases.
  (when (is (= expected-fact fact)
            "The expected and actual must be equal")
    
    (or (identical? expected-fact fact)
        (and (is (= (coll? expected-fact)
                    (coll? fact))
                 (str "not identical? with expected should mean both are collections" \newline
                      "expected fact: " expected-fact \newline
                      "fact: " fact \newline))

             (is (= (count expected-fact)
                    (count fact))
                 (str "not identical? with expected should mean both are the"
                      " same size collections" \newline
                      "expected fact: " expected-fact \newline
                      "fact: " fact \newline))

             ;; Not handling collection types not being used right now in the test.
             ;; This is only using the rules from clara.durability-rules.
             ;; If more sophisticated aggregates are used in join bindings, fact bindings,
             ;; or accumulated results in these tests case, the testing here will have to
             ;; be made more robust to show it.
             (cond
               (sequential? expected-fact)
               (and (is (sequential? fact)
                        (str "expected is sequential?" \newline
                             "expected fact: " expected-fact \newline
                             "fact: " fact \newline))
                    (mapv check-fact
                          expected-fact
                          fact))

               (set? expected-fact)
               (and (is (set? fact)
                        "expected is set?")
                    (every? #(is (identical? (expected-fact %) %)
                                 (str "the fact from one set must be found in the"
                                      " expected set and be identical? to it" \newline
                                      "expected fact: " expected-fact \newline
                                      "fact: " fact \newline))
                            fact))

               :else
               (is false
                   (str "Must find a matching comparison with the expected."
                        "  Most of the time this means the facts should be identical?" \newline
                        "expected fact: " expected-fact \newline
                        "fact: " fact \newline)))))))

(defn durability-test
  "Test runner to run different implementations of d/ISessionSerializer."
  [serde-type]
  (let [s (mk-session 'clara.durability-rules)

        thresh50 (dr/->Threshold 50)
        temp50 (->Temperature 50 "MCI")
        temp40 (->Temperature 40 "LAX")
        temp30 (->Temperature 30 "SAN")
        temp20 (->Temperature 20 "CHI")
        ws50 (->WindSpeed 50 "MCI")
        ws40 (->WindSpeed 40 "LAX")
        ws10 (->WindSpeed 10 "IRK")
        fired (-> s
                  (insert thresh50
                          temp50
                          temp40
                          temp30
                          temp20
                          ws50
                          ws40
                          ws10)
                  fire-rules)

        unpaired-res (query fired dr/unpaired-wind-speed)
        cold-res (query fired dr/cold-temp)
        hot-res (query fired dr/hot-temp)
        temp-his-res (query fired dr/temp-his)
        temps-under-thresh-res (query fired dr/temps-under-thresh)

        create-serializer (fn [stream]
                            ;; Currently only one.
                            (condp = serde-type
                              :fressian (df/create-session-serializer stream)))
        
        rulebase-serializer (create-serializer (jio/output-stream rulebase-data))
        session-serializer (create-serializer (jio/output-stream session-data))

        holder (atom [])
        mem-serializer (->LocalMemorySerializer holder)]

    ;; Serialize the data.  Store the rulebase seperately.  This is likely to be the most common usage.
    
    (d/serialize-rulebase fired
                          rulebase-serializer)
    (d/serialize-session-state fired
                               session-serializer
                               mem-serializer)

    (let [rulebase-serializer (create-serializer (jio/input-stream rulebase-data))
          session-serializer (create-serializer (jio/input-stream session-data))

          restored-rulebase (d/deserialize-rulebase rulebase-serializer)
          restored (d/deserialize-session-state session-serializer
                                                mem-serializer
                                                {:base-rulebase restored-rulebase})
          
          r-unpaired-res (query restored dr/unpaired-wind-speed)
          r-cold-res (query restored dr/cold-temp)
          r-hot-res (query restored dr/hot-temp)
          r-temp-his-res (query restored dr/temp-his)
          r-temps-under-thresh-res (query restored dr/temps-under-thresh)

          facts @(:holder mem-serializer)]

      (testing "Ensure the queries return same before and after serialization"
        (is (= (frequencies [{:?ws (dr/->UnpairedWindSpeed ws10)}])
               (frequencies unpaired-res)
               (frequencies r-unpaired-res)))

        (is (= (frequencies [{:?c (->Cold 20)}])
               (frequencies cold-res)
               (frequencies r-cold-res)))

        (is (= (frequencies [{:?h (->Hot 50)}
                             {:?h (->Hot 40)}
                             {:?h (->Hot 30)}])
               (frequencies hot-res)
               (frequencies r-hot-res)))

        (is (= (frequencies [{:?his (->TemperatureHistory [50 40 30 20])}])
               (frequencies temp-his-res)
               (frequencies r-temp-his-res)))

        (is (= (frequencies [{:?tut (dr/->TempsUnderThreshold [temp40 temp30 temp20])}])
               (frequencies temps-under-thresh-res)
               (frequencies r-temps-under-thresh-res))))

      (testing "metadata is preserved on rulebase nodes"
        (let [node-with-meta (->> s
                                  eng/components
                                  :rulebase
                                  :id-to-node
                                  vals
                                  (filter #(meta %))
                                  first)
              restored-node-with-meta (-> restored-rulebase
                                          :id-to-node
                                          (get (:id node-with-meta)))]
          (is (= (meta node-with-meta) (meta restored-node-with-meta)))))

      (testing (str "facts given to serialize-facts of IWorkingMemorySerializer"
                    " from ISessionSerializer have identity relationships"
                    " retained and accumulated values present.")
        ;; Unfortunately what seems like the best way to test this right now is to just manually
        ;; write out the whole expectation.  This is brittle, but hopefully doesn't change
        ;; that often.
        (let [cold20 (-> cold-res first :?c)
              unpaired-ws10 (-> unpaired-res first :?ws)
              temp-his (-> temp-his-res first :?his)
              temps-under-thresh (-> temps-under-thresh-res first :?tut)
              [hot30 hot40 hot50] (->> hot-res (map :?h) (sort-by :temperature))

              ;; All of these facts must have an identical? relationship (same object references)
              ;; as the actual facts being tested against.
              expected-facts [temp50
                              temp40
                              temp30
                              temp20
                              [temp50 temp40 temp30 temp20]
                              cold20
                              unpaired-ws10
                              temp-his
                              ws50
                              ws40
                              ws10
                              thresh50
                              temps-under-thresh
                              hot40
                              hot30
                              hot50
                              [temp40 temp30 temp20]]]
          
          (is (= (count expected-facts)
                 (count facts))
              (str "expected facts:" \newline
                   (vec expected-facts) \newline
                   "actual facts:" \newline
                   (vec facts)))
          
          (doseq [i (range (count expected-facts))
                  :let [expected-fact (nth expected-facts i)
                        fact (nth facts i)]]
            (check-fact expected-fact fact)))))))
 
(deftest test-durability-fressian-serde
  (durability-test :fressian))
