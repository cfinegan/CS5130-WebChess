(ns chess.events
  (:require
   [re-frame.core :as re-frame]
   [chess.db :as db]
   [chess.lobby.events :as lobby-events]
   [chess.match.events :as match-events]
   [chess.chess :as chess]
   [chess.db :as db]
   ))

(defn write-json-str [obj]
  (.stringify js/JSON (clj->js obj)))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   (.send db/conn (write-json-str {:type :refresh-game-list}))
   db/default-db))

(re-frame/reg-event-db
 ::return-to-lobby
 (fn [_ _]
   (do
     (.send db/conn
            (write-json-str
             {:type :refresh-game-list}))
     {:page :lobby
      :match nil
      :lobby {:games nil
              :joining-game? false
              :waiting-for-join? false
              :waiting-for-game-list? true
              :rules nil}})))
    
(re-frame/reg-event-fx
 ::join-game
 (fn [cofx [_ msg]]
   (let [team (:team msg)
         rules (:rules msg)
         game-id (:game-id msg)
         game-name (:name msg)]
     {:db (assoc (:db cofx)
                 :page :match
                 :lobby nil
                 :match {:history [chess/default-game]
                         :team (keyword team)
                         :server-forced-undo? false
                         :server-forced-undo-msg nil
                         :selection nil
                         :bad-select? nil
                         :game-over? false
                         :leaving? false
                         :forfeit? false
                         :undo? false
                         :opponent-undo? false
                         :game-id game-id
                         :game-name game-name
                         :rules rules})})))

(defn read-json-response [r]
  (js->clj (.parse js/JSON (.-data r)) :keywordize-keys true))

(defn client-handle-response [r]
  (let [msg (read-json-response r)
        type (keyword (:type msg))]
    (cond
      (= type :bad-find-game) (re-frame/dispatch [::lobby-events/bad-find-game])
      (= type :bad-create-game) (re-frame/dispatch [::lobby-events/bad-create-game])
      (= type :game-list) (re-frame/dispatch [::lobby-events/new-game-list msg])
      (= type :invalid-move) (re-frame/dispatch [::match-events/invalid-move msg])
      (= type :game-over) (re-frame/dispatch [::match-events/game-over])
      (= type :forfeit) (re-frame/dispatch [::match-events/game-over])
      (= type :opponent-moved) (re-frame/dispatch [::match-events/opponent-moved msg])
      (= type :bad-leave-request) (re-frame/dispatch [::match-events/bad-leave])
      (= type :bad-forfeit-request) (re-frame/dispatch [::match-events/bad-forfeit])
      (= type :bad-undo-request) (re-frame/dispatch [::match-events/bad-undo])
      (= type :bad-undo-response) (re-frame/dispatch [::match-events/bad-undo])
      (= type :undo-request) (re-frame/dispatch [::match-events/undo-request])
      (= type :undo-response) (re-frame/dispatch [::match-events/undo-response msg])
      (= type :leave) (re-frame/dispatch [::return-to-lobby])
      (= type :new-game) (re-frame/dispatch [::join-game msg])
      :else (throw (js/Error. (str "invalid response type: " type))))))
