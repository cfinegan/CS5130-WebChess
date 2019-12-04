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
     [:b "WebChess"]
     [:br]
     (if finding-game?
       "Searching..."
       "Find a game")
     [:br]
     [:br]
     [:input {:type :checkbox
              :on-change (fn []
                           (re-frame/dispatch
                            [::events/boolean-rule-click
                             :self-check?]))
              :name "self-check-box"}]
     " "
     [:label {:for "self-check-box"} "Self-check"]
     [:br]
     [:input {:type :checkbox
              :on-change (fn []
                           (re-frame/dispatch
                            [::events/boolean-rule-click
                             :en-passant?]))
              :name "en-passant-box"}]
     " "
     [:label {:for "en-passant-box"} "En passant"]
     [:br]
     [:br]
     [:button {:class "btn btn-dark"
               :on-click on-find-game-click
               :disabled finding-game?}
      "Find game"]
     ]))
