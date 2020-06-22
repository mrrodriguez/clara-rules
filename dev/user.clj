(ns user
  (:require [figwheel.main.api]
            [sc.api]
            [sc.api.logging :as sc.log]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; scope-capture

;;; Workaround for CLJS
;;; https://github.com/vvvvalvalval/scope-capture/issues/24#issuecomment-410271871
(sc.log/register-cs-logger
 ::sc.log/log-spy-cs
 (fn [cs]
   nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Debug figwheel-main compile errors

(comment
  (do
    (require 'figwheel.main)
    (in-ns 'figwheel.main))

  (def cfg (let [{:keys [id] :as build} (start-build-arg->build-options "dev")]
             (cond-> {:options (:options build)
                      ::join-server? false}
               id    (assoc ::build (dissoc build :options))
               (= :none (get-in build [:options :optimizations] :none)) (assoc-in [::config :mode] :repl))))

  (let [{:keys [options repl-options repl-env-options ::config] :as b-cfg} (add-default-system-app-handler (update-config cfg))
        {:keys [mode pprint-config ::build-once]} config
        repl-env-fn cljs.repl.figwheel/repl-env
        repl-env (apply repl-env-fn (mapcat identity repl-env-options))
        cenv (cljs.env/default-compiler-env options)
        repl-options (assoc repl-options :compiler-env cenv)
        ]
    (binding [*base-config* cfg
              *config* b-cfg]
      (cljs.env/with-compiler-env cenv
        (binding [cljs.repl/*repl-env* repl-env
                  figwheel.core/*config* (select-keys config [:hot-reload-cljs
                                                              :broadcast-reload
                                                              :reload-dependents
                                                              :build-inputs
                                                              :watch-dirs])]
          (try
            (try
              (let [{:keys [watch-dirs mode ::build-once ::build-inputs]} config
                    id (:id (::build *config*) "unknown")]
                (build-cljs id (apply bapi/inputs build-inputs) options cenv))
              (catch Throwable t
                (log/error t)
                (throw t)))
            (finally
              ;; these are the blocking states that we want to clean up after
              (when (or (get b-cfg ::join-server? true)
                        (and
                         (not (in-nrepl?))
                         (= mode :repl)))
                (fww/stop!)
                (remove-watch cenv :figwheel.core/watch-hook)
                (swap! build-registry dissoc (get-in *config* [::build :id])))))))))

  ;; Look at exec points if logging didn't work due to `*out*` re-binding.
  @sc.impl.db/db
  )
