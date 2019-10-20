(ns chess.events
  (:require
   [re-frame.core :as re-frame]
   [chess.db :as db]
   ))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-fx
 :board-click
 (fn [cofx [_ x y]]
   ;; TODO: Figure out who's turn it is, whether or not this
   ;; is a click to select a piece or confirm a move, update
   ;; db and board visual respectively.
   {}))
