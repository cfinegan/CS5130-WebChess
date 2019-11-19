(ns chess.cl.subs
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]))

(re-frame/reg-sub
 ::game
 (fn [db]
   (:game db)))

(re-frame/reg-sub
 ::finding-game?
 (fn [db]
   (:finding-game? db)))

(re-frame/reg-sub
 ::message
 (fn [db]
   (:message db)))

(re-frame/reg-sub
 ::history
 (fn [db]
   (let [game (:game db)]
     (if game
       (:history game)
       nil))))

(re-frame/reg-sub
 ::current-game
 (fn [db]
   (let [game (:game db)]
     (if game
       (last (:history game))
       nil))))

(re-frame/reg-sub
 ::board
 (fn [db]
   (let [game (:game db)]
     (if game
       (:board (last (:history game)))
       nil))))

(re-frame/reg-sub
 ::captures
 (fn [db]
   (let [game (:game db)]
     (if game
       (:captures (last (:history game)))
       nil))))

(re-frame/reg-sub
 ::selection
 (fn [db]
   (:selection db)))

(re-frame/reg-sub
 ::active-team
 (fn [db]
   (let [game (:game db)]
     (if game
       (if (= 0 (mod (count (:history game)) 2))
         chess/BLACK
         chess/WHITE)
       nil))))

(re-frame/reg-sub
 ::last-move
 (fn [db]
   (let [game (:game db)]
     (if game
       (:last-move (last (:history db)))
       nil))))

(re-frame/reg-sub
 ::undo?
 (fn [db]
   (let [game (:game db)]
     (if game
       (:undo? game)
       nil))))
