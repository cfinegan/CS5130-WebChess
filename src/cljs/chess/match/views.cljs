(ns chess.match.views
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]
   [chess.macros :refer-macros [forv]]
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

(defn tile-class [pos selection]
  (let [{sel-pos :pos
         valid-moves :valid-moves
         vuln-moves :vuln-moves
         capture-moves :capture-moves} selection]
    (cond (= sel-pos pos) "tile-selected"
          (and (some #(= pos %) vuln-moves)
               (some #(= pos %) capture-moves)) "tile-vuln-capture"
          (some #(= pos %) vuln-moves) "tile-vuln"
          (some #(= pos %) capture-moves) "tile-capture"
          (some #(= pos %) valid-moves) "tile-valid"
          (= 0 (mod (+ (:x pos) (:y pos)) 2)) "tile-dark"
          :else "tile-light")))

(defn make-board-on-click [x y]
  (fn [e]
    (.preventDefault e)
    (re-frame/dispatch [::events/board-click x y])))

(defn board-panel []
  (let [history @(re-frame/subscribe [::subs/history])
        selection @(re-frame/subscribe [::subs/selection])
        team @(re-frame/subscribe [::subs/team])
        board (:board (last history))
        rotate? (= team chess/BLACK)
        idxs (if rotate? (reverse (range 8)) (range 8))]
    [:div
     [:table.board-pane
      `[:tbody
        ~@(forv [i idxs]
            `[:tr
              ~@(forv [j idxs]
                  (let [pos (chess/->Coord j i)
                        piece (board pos)
                        bg-class (tile-class pos selection)]
                    [:td {:on-click (make-board-on-click j i)
                          :class bg-class}
                     (html-str (if piece
                                 (piece->unicode (:team piece) (:type piece))
                                 "&nbsp;"))]))])]]]))

(def type-order
  [chess/PAWN chess/ROOK chess/BISHOP chess/KNIGHT chess/QUEEN])

(defn captures-panel [team]
  (let [history @(re-frame/subscribe [::subs/history])
        game (last history)
        caps (:captures game)]
    `[:ul
      ~@(forv [type type-order]
          (let [num (caps [team type] 0)]
            (when true ;;(not (= 0 num))
              [:li (html-str (piece->unicode team type)) " x " num])))]))

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

(defn undo-panel-on-click [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/request-undo-click]))

(defn undo-panel []
  (let [undo? @(re-frame/subscribe [::subs/undo?])
        history @(re-frame/subscribe [::subs/history])]
    [:button
     {:class "btn btn-dark"
      :on-click undo-panel-on-click
      :disabled (or undo?
                    (<= (count history) 1))}
     "Request undo"]))

(defn opponent-undo-panel-on-click-accept [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/respond-undo-click true]))

(defn opponent-undo-panel-on-click-reject [e]
  (.preventDefault e)
  (re-frame/dispatch [::events/respond-undo-click false]))

(defn opponent-undo-panel-accept []
  (let [undo? @(re-frame/subscribe [::subs/undo?])]
    [:button
     {:class "btn btn-dark"
      :on-click opponent-undo-panel-on-click-accept
      :disabled undo?}
     "Accept undo"]))

(defn opponent-undo-panel-reject []
  (let [undo? @(re-frame/subscribe [::subs/undo?])]
    [:button
     {:class "btn btn-dark"
      :on-click opponent-undo-panel-on-click-reject
      :disabled undo?}
     "Reject undo"]))
     
(defn main-panel []
  (let [game-id @(re-frame/subscribe [::subs/game-id])
        game-over? @(re-frame/subscribe [::subs/game-over?])
        opponent-undo? @(re-frame/subscribe [::subs/opponent-undo?])]
    [:div
     [:div.row [:div.col [:b "Game #" game-id]]]
     [:div.row [:div.col (whos-turn-panel)]]
     [:div.row.pt-3.pb-3
      [:div.col (board-panel)]
      [:div.col.captures
       (captures-panel chess/WHITE)
       [:hr]
       (captures-panel chess/BLACK)]]
     [:div.row
      [:div.col
       (if game-over?
         (leave-panel)
         [:span
          (if opponent-undo?
            [:span (opponent-undo-panel-accept) " " (opponent-undo-panel-reject)]
            [:span (undo-panel)])
          " "
          (forfeit-panel)])]]]))
