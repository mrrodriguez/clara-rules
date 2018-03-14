(ns clara.test-rules-data
  (:require [clara.rules]
            #?(:clj [clara.rules.testfacts]
               :cljs  [clara.rules.testfacts :refer [Temperature WindSpeed ColdAndWindy]]))
  #?(:cljs (:require-macros [clara.test-rules-data]))
  #?(:clj (:import [clara.rules.testfacts
                    Temperature
                    WindSpeed
                    ColdAndWindy])))

(def the-rules
  [{:doc "Rule to determine whether it is indeed cold and windy."
    :name "clara.test-rules-data/is-cold-and-windy-data"
    :lhs `[{:type Temperature
            :constraints ~'[(< temperature 20)
                            (== ?t temperature)]}
           {:type WindSpeed
            :constraints ~'[(> windspeed 30)
                            (== ?w windspeed)]}]
    :rhs '(clara.rules/insert! (clara.rules.testfacts/->ColdAndWindy ?t ?w))}

   {:name "clara.test-rules-data/find-cold-and-windy-data"
    :lhs `[{:fact-binding :?fact
            :type ColdAndWindy
            :constraints []}]
    :params #{}}])

(defmacro weather-rules
  "Return some weather rules"
  []
  `'~the-rules)
