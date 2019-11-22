(ns chess.match.subs
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]
   ))

(defn reg-match-sub [id f]
  (re-frame/reg-sub id (fn [db] (f (:match db)))))

(reg-match-sub
 ::selection
 (fn [db]
   (:selection db)))

(reg-match-sub
 ::history
 (fn [db]
   (:history db)))

(reg-match-sub
 ::team
 (fn [db]
   (:team db)))
