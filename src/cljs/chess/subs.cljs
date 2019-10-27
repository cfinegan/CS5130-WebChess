(ns chess.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 ::history
 (fn [db]
   (:history db)))

(re-frame/reg-sub
 ::game
 (fn [db]
   (last (:history db))))

(re-frame/reg-sub
 ::board
 (fn [db]
   (:board (last (:history db)))))

(re-frame/reg-sub
 ::captures
 (fn [db]
   (:captures (last (:history db)))))

(re-frame/reg-sub
 ::selection
 (fn [db]
   (:selection db)))

(re-frame/reg-sub
 ::message
 (fn [db]
   (:message db)))

(re-frame/reg-sub
 ::gameover
 (fn [db]
   (:gameover db)))
