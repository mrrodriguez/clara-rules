(ns clara.test-clear-ns-productions
  "Tests that clear-ns-productions! correction clears all vars marked as productions from the
  namespace."
  (:require [clara.tools.testing-utils :as tu]
            [clara.rules :as r]
            [clojure.test :as t]))

(t/use-fixtures :each tu/side-effect-holder-fixture)

(r/defrule rule-to-be-cleared
  [:a]
  =>
  (prn "HELLO")
  (reset! tu/side-effect-holder :before-clearing)
  (r/insert! :before-clearing))

(r/defquery query-to-be-cleared [] [?f <- :before-clearing])

#?(:clj
   (def ^:production-seq ns-production-seq-to-be-cleared
     [{:doc  "Before clearing"
       :name "clara.test-clear-ns-productions/production-seq-to-be-cleared"
       :lhs  '[{:type        :a
                :constraints []}]
       :rhs  '(clara.rules/insert! :before-clearing-seq)}]))

(r/defsession uncleared-session clara.test-clear-ns-productions :fact-type-fn identity)

(r/clear-ns-productions!)

(r/defrule rule-after-clearing
  [:a]
  =>
  (r/insert! :after-clearing))

(r/defquery query-before-clearing [] [?f <- :before-clearing])
(r/defquery query-after-clearing [] [?f <- :after-clearing])
(r/defquery query-before-clearing-seq [] [?f <- :before-clearing-seq])
(r/defquery query-after-clearing-seq [] [?f <- :after-clearing-seq])

#?(:clj
   (def ^:production-seq production-seq-after-clearing
     [{:doc  "After clearing"
       :name "clara.test-clear-ns-productions/production-seq-after-clearing"
       :lhs  '[{:type        :a
                :constraints []}]
       :rhs  '(clara.rules/insert! :after-clearing-seq)}]))

(r/defsession cleared-session clara.test-clear-ns-productions :fact-type-fn identity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TODO: Evaluate how to get these tests to work under newer CLJS compilation model differences with CLJ.
;;;; See https://github.com/cerner/clara-rules/issues/388

;;; Then tests validating what productions the respective sessions have.
#_(t/deftest cleared?
    (let [uncleared (-> uncleared-session (r/insert :a) (r/fire-rules))]
      (t/is (= :before-clearing @tu/side-effect-holder))
      (reset! tu/side-effect-holder nil))
    (let [cleared (-> cleared-session (r/insert :a) (r/fire-rules))]
      (t/testing "cleared-session should not contain any productions before (r/clear-ns-productions!)"
        (t/is (= nil @tu/side-effect-holder))
        (t/is (empty? (r/query cleared query-before-clearing)))
        #?(:clj (t/is (not-empty (r/query cleared query-after-clearing)))))
      (t/is (empty? (r/query cleared query-before-clearing-seq)))
      #?(:clj (t/is (not-empty (r/query cleared query-after-clearing-seq))))))

#_(t/deftest query-cleared?
  (let [uncleared (-> uncleared-session (r/insert :a) (r/fire-rules))
        cleared (-> cleared-session (r/insert :a) (r/fire-rules))]
    (t/is (not-empty (r/query uncleared
                              "clara.test-clear-ns-productions/query-to-be-cleared")))
    (t/is (thrown-with-msg? #?(:clj IllegalArgumentException :cljs js/Error)
                            #"clara.test-clear-ns-productions/query-to-be-cleared"
                            (r/query cleared
                                     "clara.test-clear-ns-productions/query-to-be-cleared")))))
