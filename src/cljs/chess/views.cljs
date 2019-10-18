(ns chess.views
  (:require
   [re-frame.core :as re-frame]
   [chess.subs :as subs]
   [chess.chess :as chess]
   ))

(defn piece->string [{team :team type :type}]
  (let [team-string (cond (= team chess/WHITE) "W"
                          (= team chess/BLACK) "B")
        type-string (cond (= type chess/PAWN) "P"
                          (= type chess/ROOK) "R"
                          (= type chess/KNIGHT) "T"
                          (= type chess/KING) "K"
                          (= type chess/QUEEN) "Q")]
    (str team-string type-string)))

(defn main-panel []
  (let [name (re-frame/subscribe [::subs/name])]
    [:div
     [:h1 "Hello from " @name]
     ]))

;; TODO: Make this draw the whole board and not just one piece.
(defn board-panel []
  (let [board (re-frame/subscribe [::subs/board])]
    [:div (piece->string (@board (chess/->Coord 0 0)))
     ]))
      
