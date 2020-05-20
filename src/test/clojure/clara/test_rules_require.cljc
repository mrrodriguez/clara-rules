(ns clara.test-rules-require
  (:require [clara.tools.testing-utils :as tu]
            [clara.rule-defs :as rd]
            [clara.rules.testfacts :as facts]
            [clojure.test :as t]
            [clara.rules :as r]))

;; Tests the case where rules/facts are required from a different namespace where the session is defined,
;; without an explicit :refer.
;; See https://github.com/cerner/clara-rules/issues/359

(t/use-fixtures :each tu/side-effect-holder-fixture)

(r/defsession my-session clara.rule-defs)

(t/deftest test-simple-defrule
  (let [session (r/insert my-session (facts/->Temperature 10 "MCI"))]
    (r/fire-rules session)
    (t/is (= @tu/side-effect-holder (facts/->Temperature 10 "MCI")))))

(t/deftest test-simple-query
  (let [session (-> my-session
                    (r/insert (facts/->Temperature 15 "MCI"))
                    (r/insert (facts/->Temperature 10 "MCI"))
                    (r/insert (facts/->Temperature 80 "MCI"))
                    r/fire-rules)]

    ;; The query should identify all items that were inserted and matched the
    ;; expected criteria.
    (t/is (= #{{:?t 15} {:?t 10}}
             (set (r/query session rd/cold-query))))))

(t/deftest test-simple-accumulator
  (let [session (-> my-session
                    (r/insert (facts/->Temperature 15 "MCI"))
                    (r/insert (facts/->Temperature 10 "MCI"))
                    (r/insert (facts/->Temperature 80 "MCI"))
                    r/fire-rules)]

    ;; Accumulator returns the lowest value.
    (t/is (= #{{:?t 10}}
             (set (r/query session rd/coldest-query))))))

(t/deftest test-simple-insert
  (let [session (-> my-session
                    (r/insert (facts/->Temperature 15 "MCI"))
                    (r/insert (facts/->WindSpeed 45 "MCI"))
                    (r/fire-rules))]

    (t/is (= #{{:?fact (facts/->ColdAndWindy 15 45)}}
             (set
              (r/query session rd/find-cold-and-windy))))))

