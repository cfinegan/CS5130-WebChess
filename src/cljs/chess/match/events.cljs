(ns chess.match.events
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]
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
   {:db (assoc (:db cofx) :selection {:pos (chess/->Coord x y)})}))
