(ns chess.events
  (:require
   [re-frame.core :as re-frame]
   [chess.db :as db]
   [chess.lobby.events :as lobby-events]
   [chess.match.events :as match-events]
   [chess.chess :as chess]
   ))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-fx
 ::join-game
 (fn [cofx [_ msg]]
   (let [team (:team msg)]
     {:db (assoc (:db cofx)
                 :page :match
                 :lobby nil
                 :match {:history [chess/default-game]
                         :team (keyword team)
                         :server-forced-undo? false
                         :selection nil
                         :game-over? false})})))

(defn read-json-response [r]
  (js->clj (.parse js/JSON (.-data r)) :keywordize-keys true))

(defn client-handle-response [r]
  (let [msg (read-json-response r)
        type (keyword (:type msg))]
    (cond
      (= type :bad-find-game) (re-frame/dispatch [::lobby-events/bad-find-game])
      (= type :invalid-move) (re-frame/dispatch [::match-events/invalid-move])
      (= type :game-over) (re-frame/dispatch [::match-events/game-over])
      (= type :opponent-moved) (re-frame/dispatch [::match-events/opponent-moved msg])
      (= type :new-game) (re-frame/dispatch [::join-game msg])
      :else (throw (js/Error. (str "invalid response type: " type))))))

(set! (.-onmessage db/conn) client-handle-response)
