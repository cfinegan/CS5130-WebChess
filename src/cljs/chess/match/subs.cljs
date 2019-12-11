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
 ::bad-select?
 (fn [db]
   (:bad-select? db)))

(reg-match-sub
 ::history
 (fn [db]
   (:history db)))

(reg-match-sub
 ::team
 (fn [db]
   (:team db)))

(reg-match-sub
 ::check?
 (fn [db]
   (:check? db)))

(reg-match-sub
 ::game-over?
 (fn [db] 
   (:game-over? db)))

(reg-match-sub
 ::leaving?
 (fn [db]
   (:leaving? db)))

(reg-match-sub
 ::forfeit?
 (fn [db]
   (:forfeit? db)))

(reg-match-sub
 ::server-forced-undo?
 (fn [db]
   (:server-forced-undo? db)))

(reg-match-sub
 ::server-forced-undo-msg
 (fn [db]
   (:server-forced-undo-msg db)))

(reg-match-sub
 ::undo?
 (fn [db]
   (:undo? db)))

(reg-match-sub
 ::opponent-undo?
 (fn [db]
   (:opponent-undo? db)))

(reg-match-sub
 ::game-id
 (fn [db]
   (:game-id db)))

(reg-match-sub
 ::game-name
 (fn [db]
   (:game-name db)))

(reg-match-sub
 ::rules
 (fn [db]
   (:rules db)))

(reg-match-sub
 ::active-team
 (fn [db]
   (let [history (:history db)]
     (if (= 0 (mod (count history) 2))
       chess/BLACK
       chess/WHITE))))
