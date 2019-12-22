(ns chess.match.views
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]
   [chess.match.subs :as subs]
   [chess.match.events :as events]
   ))

(defn piece->unicode [team type]
  (cond (= team chess/WHITE)
        (cond (= type chess/KING) "&#x2654;"
              (= type chess/QUEEN) "&#x2655;"
              (= type chess/ROOK) "&#x2656;"
              (= type chess/BISHOP) "&#x2657;"
              (= type chess/KNIGHT) "&#x2658;"
              (= type chess/PAWN) "&#x2659;")
        (= team chess/BLACK)
        (cond (= type chess/KING) "&#x265A;"
              (= type chess/QUEEN) "&#x265B;"
              (= type chess/ROOK) "&#x265C;"
              (= type chess/BISHOP) "&#x265D;"
              (= type chess/KNIGHT) "&#x265E;"
              (= type chess/PAWN) "&#x265F;")))

(defn html-str [str]
  [:span {:dangerouslySetInnerHTML {:__html str}}])

(defn set-style-timeout! [pos]
  (js/setTimeout
   (fn []
     (let [elem (first (array-seq (.getElementsByClassName
                             js/document "tile-bad-select")))
           class (if (= 0 (mod (+ (:x pos) (:y pos)) 2))
                   "tile-dark"
                   "tile-light")]
       (.setAttribute elem "class" class)))
   250))

(defn tile-class [pos selection bad-select last-move]
  (let [dark? (= 0 (mod (+ (:x pos) (:y pos)) 2))
        {sel-pos :pos
         valid-moves :valid-moves
         vuln-moves :vuln-moves
         capture-moves :capture-moves} selection]
    (cond (= sel-pos pos) "tile-selected"
          (and bad-select (= bad-select pos))
          (do (set-style-timeout! pos) "tile-bad-select")
          (and (some #(= pos %) vuln-moves)
               (some #(= pos %) capture-moves)) "tile-vuln-capture"
          (some #(= pos %) vuln-moves) "tile-vuln"
          (some #(= pos %) capture-moves) "tile-capture"
          (some #(= pos %) valid-moves) "tile-valid"
          (or (= (:from last-move) pos) (= (:to last-move) pos))
          (if dark? "tile-dark last-move" "tile-light last-move")
          dark? "tile-dark"
          :else "tile-light")))

(defn make-board-on-click [x y]
  (fn [e]
    (.preventDefault e)
    (re-frame/dispatch [::events/board-click x y])))

(defn board-tile [pos piece class]
  [:td {:on-click (make-board-on-click (:x pos) (:y pos))
        :class class}
   (html-str (if piece
               (piece->unicode (:team piece) (:type piece))
               "&nbsp;"))])

(defn board-panel []
  (let [history @(re-frame/subscribe [::subs/history])
        selection @(re-frame/subscribe [::subs/selection])
        bad-select? @(re-frame/subscribe [::subs/bad-select?])
        team @(re-frame/subscribe [::subs/team])
        game (last history)
        board (:board game)
        last-move (:last-move game)
        rotate? (= team chess/BLACK)
        idxs (if rotate? (reverse (range 8)) (range 8))]
    [:div
     [:table.board-pane
      (into
       [:tbody]
       (for [i idxs]
         (into
          [:tr]
          (for [j idxs]
            (let [pos (chess/->Coord j i)
                  piece (board pos)
                  class (tile-class pos selection bad-select? last-move)]
              [board-tile pos piece class])))))]]))

(def type-order
  [chess/PAWN chess/ROOK chess/BISHOP chess/KNIGHT chess/QUEEN])

(defn captures-panel [team]
  (let [history @(re-frame/subscribe [::subs/history])
        game (last history)
        caps (:captures game)]
    (into
     [:ul.captures-list
      (for [type type-order]
        (let [num (caps [team type] 0)]
          (when true
            [:li (html-str (piece->unicode team type)) " x " num])))])))

(defn whos-turn-panel []
  (let [team @(re-frame/subscribe [::subs/team])
        active-team @(re-frame/subscribe [::subs/active-team])
        check? @(re-frame/subscribe [::subs/check?])
        rules @(re-frame/subscribe [::subs/rules])
        game-over? @(re-frame/subscribe [::subs/game-over?])
        server-forced-undo? @(re-frame/subscribe [::subs/server-forced-undo?])
        server-forced-undo-msg @(re-frame/subscribe [::subs/server-forced-undo-msg])
        undo? @(re-frame/subscribe [::subs/undo?])
        opponent-undo? @(re-frame/subscribe [::subs/opponent-undo?])]
    [:div
     (if game-over?
       (let [history @(re-frame/subscribe [::subs/history])
             forfeit? @(re-frame/subscribe [::subs/forfeit?])]
         (if (or (chess/check-mate? rules team history)
                 (= (chess/winner (:captures (last history)))
                    (chess/other-team team))
                 forfeit?)
           "Game is over (you lost)."
           "Game is over (you won)."))
       (if (or undo? opponent-undo?)
         (cond
           (and undo? opponent-undo?) "Undoing the move."
           undo? "Waiting for opponent to accept your undo request."
           opponent-undo? "Opponent requested an undo, do you accept?")
         (if (= team active-team)
           (let [message (if server-forced-undo?
                           (str server-forced-undo-msg ", try again")
                           "It's your turn")]
             (if check?
               (str message " (you are in check).")
               (str message ".")))
           "It's your opponent's turn, please wait.")))]))

(defn leave-panel-on-click [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/leave-game-click]))

(defn leave-panel []
  (let [leaving? @(re-frame/subscribe [::subs/leaving?])]
    [:button
     {:class "btn btn-dark"
      :on-click leave-panel-on-click
      :disabled leaving?}
     "Return to lobby"]))

(defn forfeit-panel-on-click [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/forfeit-game-click]))

(defn forfeit-panel [] 
  (let [forfeit? @(re-frame/subscribe [::subs/forfeit?])]
    [:button
     {:class "btn btn-dark"
      :on-click forfeit-panel-on-click
      :disabled forfeit?}
     "Forfeit"]))

(defn undo-request-on-click [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/request-undo-click]))

(defn undo-accept-on-click [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/respond-undo-click true]))

(defn undo-reject-on-click [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/respond-undo-click false]))

(defn undo-panel []
  (let [undo? @(re-frame/subscribe [::subs/undo?])
        opponent-undo? @(re-frame/subscribe [::subs/opponent-undo?])
        history @(re-frame/subscribe [::subs/history])]
    (if opponent-undo?
      [:span
       [:button {:class "btn btn-dark"
                 :on-click undo-accept-on-click
                 :disabled undo?}
        "Accept undo"]
       " "
       [:button {:class "btn btn-dark"
                 :on-click undo-reject-on-click
                 :disabled undo?}
        "Reject undo"]]
      [:button {:class "btn btn-dark"
                :on-click undo-request-on-click
                :disabled (or undo? (<= (count history) 1))}
       "Request undo"])))

(def legend-panel-info
  [["#2eccfa" "A valid move"]
   ["#fa5858" "A vulnerable move"]
   ["#2efe64" "An opportunity"]
   ["#b404ae" "An opportunity with a cost"]])

(defn on-legend-header-click [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/legend-toggle]))

(defn legend-panel []
  (let [rules @(re-frame/subscribe [::subs/rules])
        minimized? @(re-frame/subscribe [::subs/legend-minimized?])
        header-arrow (if minimized? "&#x25B8;" "&#x25BE;")]
    (when (:color-tiles? rules)
      [:div.pt-3.pb-3
       [:hr]
       [:h3.mb-0.p-1.pl-2.legend-header
        {:on-click on-legend-header-click}
        (html-str header-arrow) " color legend"]
       (when (not minimized?)
         [:table.mt-0.border.legend-table
          (into
           [:tbody]
           (for [[color desc] legend-panel-info]
             [:tr
              [:td {:class "legend-color-col black-border"
                    :style {:background-color color}}
               (html-str "&nbsp;")]
              [:td.pl-3.black-border desc]]))])])))

(defn main-panel []
  (let [game-name @(re-frame/subscribe [::subs/game-name])
        game-over? @(re-frame/subscribe [::subs/game-over?])
        opponent-undo? @(re-frame/subscribe [::subs/opponent-undo?])]
    [:div
     [:div.row [:div.col [:h1 game-name]]]
     [:hr]
     [:div.row [:div.col [whos-turn-panel]]]
     [:div.row.pt-3.pb-3
      [:div.col [board-panel]]
      [:div.col.captures
       [captures-panel chess/WHITE]
       [:hr]
       [captures-panel chess/BLACK]]]
     [:div.row
      [:div.col
       (if game-over?
         [leave-panel]
         [:span [undo-panel] " " [forfeit-panel]])]]
     [:div.row [:div.col [legend-panel]]]]))
