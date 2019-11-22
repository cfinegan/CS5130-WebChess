(ns chess.lobby.events
  (:require
   [re-frame.core :as re-frame]
   [chess.db :as db]
   ))

(defn write-json-str [obj]
  (.stringify js/JSON (clj->js obj)))

(defn reg-lobby-event-fx [id f]
  (re-frame/reg-event-fx
   id
   (fn [cofx fx-vec]
     (let [db (:db cofx)
           cofx* (assoc cofx :db (:lobby db))
           result (f cofx* fx-vec)]
       (assoc result :db (assoc db :lobby (:db result)))))))

(reg-lobby-event-fx
 ::find-game
 (fn [cofx _]
   (do
     (comment (.send db/conn
                     (write-json-str
                      {:type :find-game
                       :rules {:self-check? true
                               :en-passant? true}})))
     {:db (assoc (:db cofx) :finding-game? true)})))
