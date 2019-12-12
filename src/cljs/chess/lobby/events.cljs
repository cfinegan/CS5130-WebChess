(ns chess.lobby.events
  (:require
   [re-frame.core :as re-frame]
   [chess.db :as db]
   [chess.chess :as chess]
   ))

(defn write-json-str [obj]
  (.stringify js/JSON (clj->js obj)))

(def lobby-path (re-frame/path :lobby))

(re-frame/reg-event-fx
 ::find-game
 [lobby-path]
 (fn [cofx [_ g]]
   (let [db (:db cofx)
         rules (:rules db)]
     (when-not (:joining-game? db)
       (.send db/conn
              (write-json-str
               {:type :find-game
                :id (:id g)
                :name (:name g)
                :rules (:rules g)}))
       {:db (assoc db :joining-game? true)}))))

(re-frame/reg-event-fx
 ::create-game
 [lobby-path]
 (fn [cofx _]
   (let [db (:db cofx)
         rules (if (:rules db)
                 nil
                 (chess/Rules. false false true))]
     (do
       {:db (assoc db :rules rules)}))))

(re-frame/reg-event-fx
 ::submit-game
 [lobby-path]
 (fn [cofx [_ game-name]]
   (let [db (:db cofx)]
     (do
       (.send db/conn
              (write-json-str
               {:type :create-game
                :name game-name
                :rules (:rules db)}))
       {:db (assoc db :waiting-for-join? true)}))))

(re-frame/reg-event-fx
 ::refresh-game-list
 [lobby-path]
 (fn [cofx _]
   (let [db (:db cofx)]
     (do
       (.send db/conn
              (write-json-str
               {:type :refresh-game-list}))
       {:db (assoc db :waiting-for-game-list? true)}))))                

(re-frame/reg-event-fx
 ::new-game-list
 [lobby-path]
 (fn [cofx [_ msg]]
   (let [db (:db cofx)
         games (:games msg)]
     {:db (assoc db
                 :games games
                 :waiting-for-game-list? false)})))

(re-frame/reg-event-fx
 ::bad-find-game
 [lobby-path]
 (fn [cofx _]
   {:db (assoc (:db cofx)
               :joining-game? false)}))

(re-frame/reg-event-fx
 ::bad-create-game
 [lobby-path]
 (fn [cofx _]
   {:db (assoc (:db cofx)
               :waiting-for-join? false)}))

(re-frame/reg-event-fx
 ::boolean-rule-click
 [lobby-path]
 (fn [cofx [_ key]]
   (let [db (:db cofx)
         rules (:rules db)]
     {:db (assoc db
                 :rules (update-in rules [key] not))})))
                                      
