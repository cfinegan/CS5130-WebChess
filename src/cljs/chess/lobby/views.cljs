(ns chess.lobby.views
  (:require
   [re-frame.core :as re-frame]
   [chess.lobby.subs :as subs]
   [chess.lobby.events :as events]
   ))

(defn on-find-game-click [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/find-game]))

(defn main-panel []
  (let [finding-game? @(re-frame/subscribe [::subs/finding-game?])]
    [:div
     (if finding-game?
       "Searching..."
       "Click to find a game")
     [:br]
     [:button {:class "btn btn-dark"
               :on-click on-find-game-click
               :disabled finding-game?}
      "find game"]]))
