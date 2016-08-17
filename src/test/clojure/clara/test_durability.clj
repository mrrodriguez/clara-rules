(ns clara.test-durability
  (:require [clara.rules :refer :all]
            [clara.rules.dsl :as dsl]
            [clara.rules.engine :as eng]
            [clara.rules.durability :as d]
            [clara.rules.durability.print-method :as dpm]
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

(defn- restore-session
  "Wrapper for d/restore-session-state that validates the session can be serialized."
  [session session-state]
  (d/restore-session-state session (-> (pr-str session-state)
                                       (read-string))))

(defn- has-fact? [token fact]
  (some #{fact} (map first (:matches token))))

(deftest test-restore-simple-query

  (let [cold-query (dsl/parse-query [] [[?t <- Temperature (< temperature 20)]])

        session (-> (mk-session [cold-query] :cache false)

                    (insert (->Temperature 10 "MCI"))
                    (fire-rules))

        session-state (d/session-state session)

        restored-session (-> (mk-session [cold-query])
                             (restore-session session-state))]

    (is (= [{:?t (->Temperature 10 "MCI")}]
           (query session cold-query)))

    (is (= [{:?t (->Temperature 10 "MCI")}]
           (query restored-session cold-query)))))


(deftest test-restore-unfired-activation
  (let [rule-output (atom nil)
        cold-rule (dsl/parse-rule [[Temperature (< temperature 20)]]
                                  (reset! rule-output ?__token__))

        session-state (-> (mk-session [cold-rule])
                          (insert (->Temperature 10 "MCI"))
                          (d/session-state))]

    ;; Rule not yet fired, so the output should be nil.
    (is (nil? @rule-output))

    ;; Restore the session and run the rule.
    (-> (mk-session [cold-rule])
        (restore-session session-state)
        (fire-rules))

    (is (has-fact? @rule-output (->Temperature 10 "MCI")))))


(deftest test-restore-fired-activation
  (let [rule-output (atom nil)
        cold-rule (dsl/parse-rule [[Temperature (< temperature 20)]]
                                  (reset! rule-output ?__token__))

        session-state (-> (mk-session [cold-rule])
                          (insert (->Temperature 10 "MCI"))
                          (fire-rules)
                          (d/session-state))]

    ;; Rule fired previously, so the output should be in place.
    (is (has-fact? @rule-output (->Temperature 10 "MCI")))

    ;; Clear the output and restore the session. It should already be activated,
    ;; so the output is not updated again.
    (reset! rule-output nil)

    (-> (mk-session [cold-rule])
        (restore-session session-state)
        (fire-rules))

    (is (nil? @rule-output))))

(deftest test-restore-accum-result
  (let [lowest-temp (acc/min :temperature :returns-fact true)
        coldest-query (dsl/parse-query [] [[?t <- lowest-temp from [Temperature]]])

        session (-> (mk-session [coldest-query])
                    (insert (->Temperature 15 "MCI")
                            (->Temperature 10 "MCI")
                            (->Temperature 80 "MCI"))
                    (fire-rules))

        session-state (d/session-state session)

        restored-session (-> (mk-session [coldest-query])
                             (restore-session session-state))]

    ;; Accumulator returns the lowest value.
    (is (= #{{:?t (->Temperature 10 "MCI")}}
           (set (query session coldest-query))
           (set (query restored-session coldest-query))))))

(deftest test-restore-truth-maintenance
  (let [cold-rule (dsl/parse-rule [[Temperature (< temperature 20) (= ?temp temperature)]]
                                  (insert! (->Cold ?temp)))

        cold-query (dsl/parse-query [] [[?cold <- Cold]])

        empty-session (mk-session [cold-rule cold-query])

        persist-and-restore (fn [session]
                              (restore-session empty-session
                                               (d/session-state session)))

        cold-result {:?cold (->Cold 10)}]

    (let [session (-> empty-session
                      (insert (->Temperature 10 "MCI"))
                      (fire-rules))

          session-state (d/session-state session)
          
          restored-session (-> empty-session
                               (restore-session session-state))]

      (is (= [{:?cold (->Cold 10)}]
             (query restored-session cold-query))
          "Query for a Cold fact from the restored session.")

      ;; Ensure the retraction propagates and removes our inserted item.
      (is (= []
             (query (retract restored-session (->Temperature 10 "MCI")) cold-query))
          "Validate that the restored session doesn't contain a Cold fact after
         the Temperature fact that inserted it in the original session is retracted."))

    (let [session (-> empty-session
                      (insert (->Temperature 10 "MCI"))
                      (insert (->Temperature 10 "MCI"))
                      (fire-rules))

          restored-session (persist-and-restore session)]

      (is (= [cold-result cold-result]
             (query restored-session cold-query))
          "Two duplicate Cold facts should be restored from the persisted session when there
           are two duplicate Temperature facts.")

      (is (= (query (retract restored-session (->Temperature 10 "MCI"))
                    cold-query)
             [cold-result])
          "When there are two duplicate Cold facts from two separate Temperature
           facts and one Temperature fact is retracted we should still have 1 Cold fact.")

      (is (= (query (-> restored-session
                        (retract (->Temperature 10 "MCI"))
                        (retract (->Temperature 10 "MCI")))
                    cold-query)
             [])
          "When two duplicate Cold facts are inserted from two duplicate Temperature facts,
           and both Temperature facts are retracted, no Cold facts should remain."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; New durability.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord LocalMemorySerializer [holder]
  d/IWorkingMemorySerializer
  (serialize-facts [_ fact-seq]
    (reset! holder fact-seq))
  (deserialize-facts [_]
    @holder))

(def rulebase-data (java.io.File/createTempFile "rulebase" ".dat"))
(def session-data (java.io.File/createTempFile "session" ".dat"))

(defn durability-test [serde-type]
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
                            (condp = serde-type
                              :print-method (dpm/create-session-serializer stream)
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
          (is (= (meta node-with-meta) (meta restored-node-with-meta))))))))

(deftest test-durability-print-method-serde
  ;;(durability-test :print-method)
  )
 
(deftest test-durability-fressian-serde
  (durability-test :fressian))
