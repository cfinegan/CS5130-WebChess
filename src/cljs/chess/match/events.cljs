(ns chess.match.events
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]
   [chess.match.subs :as subs]
   ))

(defn reg-match-event-fx [id f]
  (re-frame/reg-event-fx
   id
   (fn [cofx fx-vec]
     (let [db (:db cofx)
           cofx* (assoc cofx :db (:match db))
           result (f cofx* fx-vec)]
       (assoc result :db (assoc db :match (:db result)))))))

(reg-match-event-fx
 ::board-click
 (fn [cofx [_ x y]]
   (let [history @(re-frame/subscribe [::subs/history])
         game (last history)
         board (:board game)
         selection @(re-frame/subscribe [::subs/selection])
         team @(re-frame/subscribe [::subs/team])
         active-team @(re-frame/subscribe [::subs/active-team])
         click-pos (chess/->Coord x y)]
     (cond
       ;; Do nothing if it's the opponent's turn.
       ;; TODO: Add an invalid selection animation.
       (not (= team active-team))
       {:db (:db cofx)}
       ;; If a piece is already selected...
       selection
       (let [{sel-pos :pos valid-moves :valid-moves} selection]
         (if (= sel-pos click-pos)
           ;; Selecting the same piece twice de-selects.
           {:db (assoc (:db cofx) :selection nil)}
           ;; Else try to move the piece.
           (let [move (chess/->Move sel-pos click-pos)]
             (if (some #(= click-pos %) valid-moves)
               ;; Update game state if the move is valid
               ;; TODO: Send move to server.
               (let [new-game (chess/apply-move game move)]
                 {:db (assoc (:db cofx)
                             :history (conj history new-game)
                             :selection nil)})
               ;; Otherwise do nothing.
               ;; TODO: Add an invalid selection animation.
               {:db (:db cofx)}))))
       ;; Otherwise attempt to select.
       :else
       (let [piece (board click-pos)]
         (if (and piece (= (:team piece) team))
           ;; Only select when the space holds a friendly piece.
           (let [valid-moves (chess/valid-moves history click-pos true)]
             {:db (assoc (:db cofx) :selection {:pos click-pos
                                                :valid-moves valid-moves})})
           ;; Otherwise do nothing
           ;; TODO: Add an invalid selection animation.
           {:db (:db cofx)}))))))

