(ns clara.rule-defs
  (:require [clara.rules.accumulators :as acc]
            [clara.rules.testfacts :as tf]
            [clara.tools.testing-utils :as tu]
            [clara.rules :as r])
  #?(:clj
     (:import [clara.rules.testfacts
               Temperature
               WindSpeed
               ColdAndWindy])))

;; Accumulator for getting the lowest temperature.
(def lowest-temp (acc/min :temperature))

;; Rule definitions used for tests in clara.test-rules-require.

(r/defrule test-rule
  [?t <- #?(:clj Temperature :cljs tf/Temperature) (< temperature 20)]
  =>
  (reset! tu/side-effect-holder ?t))

(r/defquery cold-query
          []
          [#?(:clj Temperature :cljs tf/Temperature) (< temperature 20) (== ?t temperature)])


(r/defquery coldest-query
          []
          [?t <- lowest-temp :from [#?(:clj Temperature :cljs tf/Temperature)]])

(r/defrule is-cold-and-windy
         "Rule to determine whether it is indeed cold and windy."

         (#?(:clj Temperature :cljs tf/Temperature) (< temperature 20) (== ?t temperature))
         (#?(:clj WindSpeed :cljs tf/WindSpeed) (> windspeed 30) (== ?w windspeed))
         =>
         (r/insert! (tf/->ColdAndWindy ?t ?w)))

(r/defquery find-cold-and-windy
          []
          [?fact <- #?(:clj ColdAndWindy :cljs tf/ColdAndWindy)])

