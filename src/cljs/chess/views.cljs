(ns chess.views
  (:require
   [re-frame.core :as re-frame]
   [chess.subs :as subs]
   [chess.chess :as chess]
   [chess.macros :refer-macros [forv]]
   ))

(defn piece->string [{team :team type :type}]
  (let [team-string (cond (= team chess/WHITE) "W"
                          (= team chess/BLACK) "B")
        type-string (cond (= type chess/PAWN) "P"
                          (= type chess/ROOK) "R"
                          (= type chess/KNIGHT) "T"
                          (= type chess/BISHOP) "B"
                          (= type chess/KING) "K"
                          (= type chess/QUEEN) "Q")]
    (str team-string type-string)))

(defn make-on-click [x y]
  (fn [e]
    (.preventDefault e)
    (re-frame/dispatch [:board-click x y])))

(defn board-panel []
  (let [board (re-frame/subscribe [::subs/board])
        last-move (re-frame/subscribe [::subs/last-move])
        selection (re-frame/subscribe [::subs/selection])
        history (re-frame/subscribe [::subs/history])
        moves (and @selection
                   (chess/valid-moves @board @selection @history true))]
    [:div
     [:table {:border 1
              :style  {:table-layout "fixed"
                       :width "350px"
                       :height "300px"
                       :text-align "center"}}
      `[:tbody
        ~@(forv [i (range 8)]
            `[:tr
              ~@(forv [j (range 8)]
                  (let [pos (chess/->Coord j i)
                        piece (@board pos)
                        bg (cond (= pos @selection) "green"
                                 (and moves (some #(= pos %) moves)) "blue"
                                 (and @last-move
                                      (let [{fx :x fy :y} (:from @last-move)
                                            {tx :x ty :y} (:to @last-move)]
                                        (or (and (= fx j) (= fy i))
                                            (and (= tx j) (= ty i))))) "#cccccc")]
                    (if piece
                      [:td {:on-click (make-on-click j i)
                            :style {:background-color bg}}
                       (piece->string piece)]
                      [:td {:on-click (make-on-click j i)
                            :style {:background-color bg}
                            :dangerouslySetInnerHTML
                            {:__html "&nbsp;"}}])))])]]]))

(defn message-panel []
  (let [msg (re-frame/subscribe [::subs/message])]
    [:div @msg]))

(defn on-undo-click [e]
  (.preventDefault e)
  (re-frame/dispatch [:undo-click]))

(defn undo-button []
  (let [history (re-frame/subscribe [::subs/history])]
    [:div [:button {:class "btn btn-dark"
                    :on-click on-undo-click
                    :disabled (<= (count @history) 1)} "undo"]]))

(defn whos-turn-panel []
  (let [team @(re-frame/subscribe [::subs/active-team])]
    [:div "it's " (chess/team->string team) "'s turn"]))

(defn main-panel []
  [:div {:class "container mt-3 p-4 rounded"
         :style {:background-color "#eee"}}
   (message-panel)
   [:br]
   (board-panel)
   [:br]
   (undo-button)
   [:br]
   (whos-turn-panel)])
