(ns chess.match.views
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]
   [chess.macros :refer-macros [forv]]
   [chess.match.subs :as subs]
   [chess.match.events :as events]
   ))

(defn piece->unicode [{team :team type :type}]
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

(defn tile-color [x y]
  (if (= 0 (mod (+ x y) 2))
    "#f7f2f0"
    "#ffffff"))

(defn make-board-on-click [x y]
  (fn [e]
    (.preventDefault e)
    (re-frame/dispatch [::events/board-click x y])))

(defn board-panel []
  (let [history @(re-frame/subscribe [::subs/history])
        {sel-pos :pos} @(re-frame/subscribe [::subs/selection])
        team @(re-frame/subscribe [::subs/team])
        board (:board (last history))
        rotate? (= team chess/BLACK)
        idxs (if rotate? (reverse (range 8)) (range 8))]
    [:div
     [:table {:border 1
              :style {:table-layout "fixed"
                      :width "350px"
                      :height "300px"
                      :text-align "center"}}
      `[:tbody
        ~@(forv [i idxs]
            `[:tr
              ~@(forv [j idxs]
                  (let [pos (chess/->Coord j i)
                        piece (board pos)
                        bg (cond (= sel-pos pos) "#f3f781"
                                 :else (tile-color j i))]
                    [:td {:on-click (make-board-on-click j i)
                          :style {:background-color bg}
                          :dangerouslySetInnerHTML
                          {:__html (if piece
                                     (piece->unicode piece)
                                     "&nbsp;")}}]))])]]]))
    
(defn main-panel []
  [:div
   (board-panel)])
