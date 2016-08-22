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
          (is (= (meta node-with-meta) (meta restored-node-with-meta))))))))
 
(deftest test-durability-fressian-serde
  (durability-test :fressian))
