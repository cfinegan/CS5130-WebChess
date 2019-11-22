(ns chess.lobby.subs
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]
   ))

(defn reg-lobby-sub [id f]
  (re-frame/reg-sub id (fn [db] (f (:lobby db)))))

(reg-lobby-sub
 ::finding-game?
 (fn [db]
   (:finding-game? db)))
