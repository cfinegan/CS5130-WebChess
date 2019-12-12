(ns chess.db
  (:require
   [chess.chess :as chess]
   ))

(def conn nil)

(def default-db
  {:page :lobby
   :match nil
   :lobby {:games []
           :joining-game? false
           :waiting-for-join? false
           :game-name ""
           :rules nil}})
