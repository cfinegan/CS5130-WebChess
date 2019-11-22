(ns chess.subs
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]
   ))

(re-frame/reg-sub
 ::page
 (fn [db]
   (println db)
   (:page db)))
