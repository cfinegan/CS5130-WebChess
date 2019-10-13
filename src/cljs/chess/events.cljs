(ns chess.events
  (:require
   [re-frame.core :as re-frame]
   [chess.db :as db]
   ))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))
