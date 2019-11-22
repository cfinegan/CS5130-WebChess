(ns chess.match.events
  (:require
   [re-frame.core :as re-frame]
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
   (println (str "user clicked (" x ", " y ")"))
   {:db (:db cofx)}))
