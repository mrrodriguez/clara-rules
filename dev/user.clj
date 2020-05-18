(ns user
  (:require [sc.api]
            [sc.api.logging :as sc.log]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; scope-capture

;;; Workaround for CLJS
;;; https://github.com/vvvvalvalval/scope-capture/issues/24#issuecomment-410271871
(sc.log/register-cs-logger
 ::sc.log/log-spy-cs
 (fn [cs]
   nil))
