(ns clara.test-testing-utils
  (:require [clara.tools.testing-utils :as tu]
            [clara.rules :as r]
            [clara.rules.testfacts :as tf :refer [->Temperature ->Cold
                                                  #?@(:cljs [Temperature Cold])]]
            [clojure.test :as t])
  #?(:clj (:import [clara.rules.testfacts
                    Temperature
                    Cold])))

(def test-ran-atom (atom false))

;; This test fixture validates that tu/def-rules-test actually executed the test bodies it
;; is provided.  If the test bodies were not executed test-ran-atom would have a value of false
;; after test execution.
(t/use-fixtures :once (fn [t]
                        (reset! test-ran-atom false)
                        (t)
                        (t/is (true? @test-ran-atom))))

(tu/def-rules-test basic-tests
  {:rules [rule1 [[[?t <- Temperature (< temperature 0)]]
                  (r/insert! (->Cold (:temperature ?t)))]]

   :queries [query1 [[]
                     [[Cold (= ?t temperature)]]]]

   :sessions [session1 [rule1 query1] {}
              session2 [rule1 query1] {:fact-type-fn (fn [fact] :bogus)}]}

  (reset! test-ran-atom true)
  (t/is (= [{:?t -50}]
           (-> session1
               (r/insert (->Temperature -50 "MCI"))
               r/fire-rules
               (r/query query1))))

  ;; Since we validate later (outside the scope of this test) that the state
  ;; change occurred put it in the middle so that it would fail if we took either
  ;; the first or last test form, rather than all test forms.
  (reset! test-ran-atom true)

  (t/is (empty? (-> session2
                    (r/insert (->Temperature -50 "MCI"))
                    r/fire-rules
                    (r/query query1)))))

(def fire-rules-counter (atom 0))

(tu/def-rules-test test-performance-test
  {:rules [rule1 [[[?t <- Temperature (< temperature 0)]]
                  (swap! fire-rules-counter inc)]]
   :queries []
   :sessions [session1 [rule1] {}]}
  (tu/run-performance-test {:description "Simple fire-rules demonstration"
                            :func #(-> session1
                                       (r/insert (->Temperature -50 "MCI"))
                                       r/fire-rules)
                            :iterations 5
                            :mean-assertion (partial > 500)})
  (t/is (= @fire-rules-counter 5)))
