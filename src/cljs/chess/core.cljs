(ns chess.core
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [chess.events :as events]
   [chess.views :as views]
   [chess.config :as config]
   [chess.db :as db]
   ))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:prod/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/main-panel]
                  (.getElementById js/document "app")))

(defn init []
  (println "in init!!")
  (set! db/conn (js/WebSocket. "ws://127.0.0.1:8080/ws"))
  (set! (.-onopen db/conn)
        (fn [_]
          (set! (.-onmessage db/conn) events/client-handle-response)
          (re-frame/dispatch-sync [::events/initialize-db])
          (dev-setup)
          (mount-root))))
