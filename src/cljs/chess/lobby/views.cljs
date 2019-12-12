(ns chess.lobby.views
  (:require
   [re-frame.core :as re-frame]
   [reagent.core :as reagent]
   [chess.lobby.subs :as subs]
   [chess.lobby.events :as events]
   [chess.macros :refer-macros [forv]]
   ))

(defn html-str [str]
  [:span {:dangerouslySetInnerHTML {:__html str}}])

(defn on-find-game-click [g]
  (fn [e]
    (.preventDefault e)
    (re-frame/dispatch [::events/find-game g])))

(defn header-panel []
  [:h1 "WebChess"])

(defn on-create-game-click [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/create-game]))

(defn create-game-button-panel []
  (let [joining-game? @(re-frame/subscribe [::subs/joining-game?])
        rules @(re-frame/subscribe [::subs/rules])]
    [:button
     {:class "btn btn-dark"
      :on-click on-create-game-click
      :disabled joining-game?}
     "Create game"]))

(defn on-submit-game-click [game-name]
  (fn [e]
    (.preventDefault e)
    (re-frame/dispatch [::events/submit-game game-name])))

(defn submit-game-button-panel [game-name]
  (let [waiting-for-join? @(re-frame/subscribe [::subs/waiting-for-join?])]
    [:button
     {:class "btn btn-dark"
      :on-click (on-submit-game-click game-name)
      :disabled waiting-for-join?}
     "Submit"]))

(let [game-name (reagent/atom "")]
  (defn create-game-panel []
    (let [waiting-for-join? @(re-frame/subscribe [::subs/waiting-for-join?])
          rules @(re-frame/subscribe [::subs/rules])]
      [:div
       [:br]
       [:label {:for "game-name-field"} "Game name:"]
       " "
       [:input {:type :text
                :value @game-name
                :name "game-name-field"
                :on-change #(reset! game-name (-> % .-target .-value))}]
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
       (submit-game-button-panel @game-name)])))

(defn on-refresh-game-list-click [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/refresh-game-list]))

(defn refresh-game-list-button-panel []
  (let [waiting-for-game-list? @(re-frame/subscribe [::subs/waiting-for-game-list?])]
    [:button
     {:class "btn btn-dark"
      :on-click on-refresh-game-list-click
      :disabled waiting-for-game-list?}
     "Refresh"]))

(defn bool-str [b]
  (if b "enabled" "disabled"))

(defn game-list-panel []
  (let [games @(re-frame/subscribe [::subs/games])]
    [:div
     (when games
       (if (empty? games)
         "Sorry, no games! Try creating one or click 'Refresh'."
         [:table
          {:class "table table-striped"}
          [:thead
           [:tr
            [:th "Name"]
            [:th "Self-check"]
            [:th "En passant"]
            [:th "Color tiles"]]]
          `[:tbody
            ~@(forv [g games]
                (let [rules (:rules g)]
                  [:tr
                   {:on-click (on-find-game-click g)}
                   [:td (:name g)]
                   [:td (bool-str (:self-check? rules))]
                   [:td (bool-str (:en-passant? rules))]
                   [:td (bool-str (:color-tiles? rules))]]))]]))]))

(defn main-panel []
  (let [rules @(re-frame/subscribe [::subs/rules])]
    [:div
     [:div.row [:div.col (header-panel)]]
     [:div.row [:div.col
                (create-game-button-panel)
                (when-not rules " ")
                (when-not rules
                  (refresh-game-list-button-panel))]]
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
