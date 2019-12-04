(ns chess.lobby.events
  (:require
   [re-frame.core :as re-frame]
   [chess.db :as db]
   ))

(defn write-json-str [obj]
  (.stringify js/JSON (clj->js obj)))

(def lobby-path (re-frame/path :lobby))

(re-frame/reg-event-fx
 ::find-game
 [lobby-path]
 (fn [cofx _]
   (let [db (:db cofx)
         rules (:rules db)]
     (do
       (.send db/conn
              (write-json-str
               {:type :find-game
                :rules rules}))
       {:db (assoc db :finding-game? true)}))))

(re-frame/reg-event-fx
 ::bad-find-game
 [lobby-path]
 (fn [cofx _]
   {:db (assoc (:db cofx)
               :finding-game? false)}))

(re-frame/reg-event-fx
 ::boolean-rule-click
 [lobby-path]
 (fn [cofx [_ key]]
   (let [db (:db cofx)
         rules (:rules db)]
     {:db (assoc db
                 :rules (update-in rules [key] not))})))
                                      
