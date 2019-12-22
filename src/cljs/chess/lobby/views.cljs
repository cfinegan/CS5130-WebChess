(ns chess.lobby.views
  (:require
   [re-frame.core :as re-frame]
   [reagent.core :as reagent]
   [chess.lobby.subs :as subs]
   [chess.lobby.events :as events]
   ))

(defn html-str [str]
  [:span {:dangerouslySetInnerHTML {:__html str}}])

(defn on-find-game-click [g]
  (fn [e]
    (.preventDefault e)
    (re-frame/dispatch [::events/find-game g])))

(defn header-panel []
  [:div.pb-3 [:h1 "WebChess"]])

(defn on-create-game-click [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/create-game]))

(defn create-game-button-panel []
  (let [joining-game? @(re-frame/subscribe [::subs/joining-game?])
        waiting-for-join? @(re-frame/subscribe [::subs/waiting-for-join?])
        rules @(re-frame/subscribe [::subs/rules])]
    [:button
     {:class "btn btn-dark"
      :on-click on-create-game-click
      :disabled (or joining-game? waiting-for-join?)}
     (if rules "Return to Lobby" "Create game")]))

(defn make-on-submit-game-click [game-name]
  (fn [e]
    (.preventDefault e)
    (re-frame/dispatch [::events/submit-game game-name])))

(defn submit-game-button-panel []
  (let [waiting-for-join? @(re-frame/subscribe [::subs/waiting-for-join?])
        game-name @(re-frame/subscribe [::subs/game-name])]
    [:button
     {:class "btn btn-dark"
      :on-click (make-on-submit-game-click game-name)
      :disabled (or waiting-for-join? (= "" game-name))}
     (if waiting-for-join? "Waiting for an opponent..." "Submit")]))

(defn make-on-rule-click [rule]
  (fn [e]
    (.preventDefault e)
    (re-frame/dispatch [::events/boolean-rule-click rule])))

(defn make-on-rule-hover [rule]
  (fn [_]
    (re-frame/dispatch [::events/rule-mouse-over rule])))

(defn create-game-panel []
  (let [waiting-for-join? @(re-frame/subscribe [::subs/waiting-for-join?])
        game-name @(re-frame/subscribe [::subs/game-name])
        rules @(re-frame/subscribe [::subs/rules])
        on-self-check-click (make-on-rule-click :self-check?)
        on-en-passant-click (make-on-rule-click :en-passant?)
        on-color-tiles-click (make-on-rule-click :color-tiles?)]
    [:div.pt-3
     [:div.p-1
      [:label {:for "game-name-field"} "Game name:"]
      [:input.m-2 {:type :text
                   :value game-name
                   :name "game-name-field"
                   :on-change #(re-frame/dispatch
                                [::events/game-name-changed
                                 (-> % .-target .-value)])}]]
     [:div.p-1
      [:input.m-2 {:type :checkbox
                   :on-change on-self-check-click
                   :name "self-check-box"
                   :checked (:self-check? rules)}]
      [:label {:for "self-check-box"
               :on-click on-self-check-click
               :on-mouse-over (make-on-rule-hover :self-check?)
               :on-mouse-leave (make-on-rule-hover nil)}
       "Self-check"]]
     [:div.p-1
      [:input.m-2 {:type :checkbox
                   :on-change on-en-passant-click
                   :name "en-passant-box"
                   :checked (:en-passant? rules)}]
      [:label {:for "en-passant-box"
               :on-click on-en-passant-click
               :on-mouse-over (make-on-rule-hover :en-passant?)
               :on-mouse-leave (make-on-rule-hover nil)}
       "En-passant"]]
     [:div.p-1
      [:input.m-2 {:type :checkbox
                   :on-change on-color-tiles-click
                   :name "color-tiles-box"
                   :checked (:color-tiles? rules)}]
      [:label {:for "color-tiles-box"
               :on-click on-color-tiles-click
               :on-mouse-over (make-on-rule-hover :color-tiles?)
               :on-mouse-leave (make-on-rule-hover nil)}
       "Color tiles"]]
     [:div.p-2 (submit-game-button-panel)]]))

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
    [:div.pt-3.pb-3
     (when games
       (if (empty? games)
         "Sorry, no games! Try creating one or click 'Refresh'."
         [:table.border.rounded
          {:class "table table-striped"}
          [:thead
           [:tr
            [:th "Name"]
            [:th "Self-check"]
            [:th "En passant"]
            [:th "Color tiles"]]]
          (into
           [:tbody]
           (for [g games]
             (let [rules (:rules g)]
               [:tr {:on-mouse-leave (make-on-rule-hover nil)}
                [:td  {:on-mouse-over (make-on-rule-hover nil)}
                 (:name g)]
                [:td.rule-col {:on-mouse-over (make-on-rule-hover :self-check?)}
                 (bool-str (:self-check? rules))]
                [:td.rule-col {:on-mouse-over (make-on-rule-hover :en-passant?)}
                 (bool-str (:en-passant? rules))]
                [:td.rule-col {:on-mouse-over (make-on-rule-hover :color-tiles?)}
                 (bool-str (:color-tiles? rules))]
                [:td.join-col.rounded-lg {:on-click (on-find-game-click g)
                                          :on-mouse-over (make-on-rule-hover nil)}
                 "JOIN"]])))]))]))

(defn tooltip-panel []
  (let [rule @(re-frame/subscribe [::subs/tooltip])]
    (when rule
      (cond
        (= rule :self-check?)
        [:div [:hr] [:h2 "self-check"]
         "When enabled, you can put yourself in check."]
         (= rule :en-passant?)
         [:div [:hr] [:h2 "en passant"]
          "Enable or disable an obscure rule about capturing pawns."]
         (= rule :color-tiles?)
         [:div [:hr] [:h2 "color tiles"]
          "Color code potential moves according to their value."
          "This will let you know if a potential move would put your piece in danger."]))))

(defn main-panel []
  (let [rules @(re-frame/subscribe [::subs/rules])]
    [:div
     [:div.row [:div.col [header-panel]]]
     [:div.row [:div.col
                [create-game-button-panel]
                (when-not rules " ")
                (when-not rules
                  [refresh-game-list-button-panel])]]
     [:div.row
      [:div.col
       (if rules
         [create-game-panel]
         [game-list-panel])]]
     [:div.row [:div.col [tooltip-panel]]]]))
