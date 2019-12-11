(ns chess.lobby.subs
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]
   ))

(defn reg-lobby-sub [id f]
  (re-frame/reg-sub id (fn [db] (f (:lobby db)))))

(reg-lobby-sub
 ::joining-game?
 (fn [db]
   (:joining-game? db)))

(reg-lobby-sub
 ::rules
 (fn [db]
   (:rules db)))

(reg-lobby-sub
 ::games
 (fn [db]
   (:games db)))

(reg-lobby-sub
 ::waiting-for-join?
 (fn [db]
   (:waiting-for-join? db)))

(reg-lobby-sub
 ::waiting-for-game-list?
 (fn [db]
   (:waiting-for-game-list? db)))
