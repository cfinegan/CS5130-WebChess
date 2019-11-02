(ns chess.subs
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]))

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

(re-frame/reg-sub
 ::active-team
 (fn [db]
   (if (= 0 (mod (count (:history db)) 2))
     chess/BLACK
     chess/WHITE)))
