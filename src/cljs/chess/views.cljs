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
        selection (re-frame/subscribe [::subs/selection])
        moves (and @selection (chess/valid-moves @board @selection))]
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
                        bg (cond (= pos @selection) "#cccccc"
                                 (and moves (some #(= pos %) moves)) "blue")]
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
      
(defn main-panel []
  [:div
   (message-panel)
   [:br]
   (board-panel)])
