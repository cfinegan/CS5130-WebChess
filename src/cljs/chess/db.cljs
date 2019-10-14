(ns chess.db
  (:require [chess.chess :as chess]))

(def default-db
  {:name "re-frame"
   :game chess/default-game})
