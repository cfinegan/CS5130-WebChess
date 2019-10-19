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

(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])]
    [:div
     [:h1 "Hello from " @name]
     ]))

(defn board-panel []
  (let [board (re-frame/subscribe [::subs/board])]
    [:div
     [:table {:border 1
              :style  {:table-layout "fixed"
                       :width "350px"
                       :height "300px"
                       :text-align "center"}}
      `[:tbody
        ~@(forv [i (range 8)]
            `[:tr ~@(forv [j (range 8)]
                      [:td (let [piece (@board (chess/->Coord j i))]
                             (if piece (piece->string piece)
                                 {:dangerouslySetInnerHTML
                                  {:__html "&nbsp;"}}))])])]]]))
      
