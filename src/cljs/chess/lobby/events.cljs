(ns chess.lobby.events
  (:require
   [re-frame.core :as re-frame]
   [chess.db :as db]
   ))

(defn write-json-str [obj]
  (.stringify js/JSON (clj->js obj)))

(re-frame/reg-event-fx
 ::find-game
 (fn [cofx _]
   (do
     (comment (.send db/conn
            (write-json-str
             {:type :find-game
              :rules {:self-check? true
                      :en-passant? true}})))
     {:db (update (:db cofx) :lobby assoc :finding-game? true)})))
