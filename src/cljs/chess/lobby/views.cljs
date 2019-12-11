(ns chess.lobby.views
  (:require
   [re-frame.core :as re-frame]
   [chess.lobby.subs :as subs]
   [chess.lobby.events :as events]
   ))

(defn on-find-game-click [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/find-game]))

(defn header-panel []
  [:h1 "WebChess"])

(defn create-game-button-panel []
  (let [joining-game? @(re-frame/subscribe [::subs/joining-game?])
        rules @(re-frame/subscribe [::subs/rules])]
    [:button
     {:class "btn btn-dark"
      :on-click nil
      :disabled (or joining-game? rules)}
     "Create game"]))

(defn create-game-panel []
  (let [waiting-for-join? @(re-frame/subscribe [::subs/waiting-for-join?])
        rules @(re-frame/subscribe [::subs/rules])]
    [:div
     "NAME GOES HERE"
     [:br]
     "CHECK BOXES GO HERE"
     [:br]
     "SUBMIT BUTTON GOES HERE"]))

(defn game-list-panel []
  (let [games @(re-frame/subscribe [::subs/games])]
    [:div
     (if (empty? games)
       "Sorry, no games! Try creating one."
       (throw (js/Error. "Can't draw games yet!")))]))

(defn main-panel []
  (let [rules @(re-frame/subscribe [::subs/rules])]
    [:div
     [:div.row [:div.col (header-panel)]]
     [:div.row [:div.col (create-game-button-panel)]]
     [:div.row
      [:div.col
       (if rules
         (create-game-panel)
         (game-list-panel))]]]))


#_
(defn main-panel []
  (let [finding-game? @(re-frame/subscribe [::subs/finding-game?])
        rules @(re-frame/subscribe [::subs/rules])]
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
              :name "self-check-box"
              :checked (:self-check? rules)}]
     " "
     [:label {:for "self-check-box"} "Self-check"]
     [:br]
     [:input {:type :checkbox
              :on-change (fn []
                           (re-frame/dispatch
                            [::events/boolean-rule-click
                             :en-passant?]))
              :name "en-passant-box"
              :checked (:en-passant? rules)}]
     " "
     [:label {:for "en-passant-box"} "En passant"]
     [:br]
     [:input {:type :checkbox
              :on-change (fn []
                           (re-frame/dispatch
                            [::events/boolean-rule-click
                             :color-tiles?]))
              :name "color-tiles-box"
              :checked (:color-tiles? rules)}]
     " "
     [:label {:for "color-tiles-box"} "Color tiles"]
     [:br]
     [:br]
     [:button {:class "btn btn-dark"
               :on-click on-find-game-click
               :disabled finding-game?}
      "Find game"]
     ]))
