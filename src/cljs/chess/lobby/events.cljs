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
   (.send db/conn
          (write-json-str
           {:type :find-game
            :rules {:self-check? true
                    :en-passant? true}}))
   {:db (assoc (:db cofx) :finding-game? true)}))

(re-frame/reg-event-fx
 ::bad-find-game
 [lobby-path]
 (fn [cofx _]
   (println "bad find game")
   {:db (assoc (:db cofx) :finding-game? false)}))
