(ns chess.cl.core
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [chess.cl.events :as events]
   [chess.cl.views :as views]))

(defn ^:prod/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render
   [views/main-panel]
   (.getElementById js/document "app")))

(defn init []
  (re-frame/dispatch [::events/initialize-db])
  (mount-root))
