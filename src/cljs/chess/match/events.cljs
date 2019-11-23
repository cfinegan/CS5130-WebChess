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
         board (:board (last history))
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
       (let [{sel-pos :pos} selection]
         (if (= sel-pos click-pos)
           ;; Selecting the same piece twice de-selects.
           {:db (assoc (:db cofx) :selection nil)}
           ;; Otherwise do nothing
           ;; TODO: try to move the piece here
           {:db (:db cofx)}))
       ;; Otherwise attempt to select.
       :else
       (let [piece (board click-pos)]
         (if (and piece (= (:team piece) team))
           ;; Only select when the space holds a friendly piece.
           {:db (assoc (:db cofx) :selection {:pos click-pos})}
           ;; Otherwise do nothing
           ;; TODO: Add an invalid selection animation.
           {:db (:db cofx)}))))))

