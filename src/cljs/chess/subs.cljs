(ns chess.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 ::game
 (fn [db]
   (:game db)))

(re-frame/reg-sub
 ::board
 (fn [db]
   (:board (:game db))))

(re-frame/reg-sub
 ::captures
 (fn [db]
   (:captures (:game db))))
