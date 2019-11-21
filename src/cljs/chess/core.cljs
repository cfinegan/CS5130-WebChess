(ns chess.core
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [chess.events :as events]
   [chess.views :as views]
   [chess.config :as config]
   ))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:prod/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn init []
  (re-frame/dispatch [::events/initialize-db])
  (dev-setup)
  (mount-root))
