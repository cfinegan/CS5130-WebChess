(ns chess.db
  (:require [chess.chess :as chess]))

(def default-db
  {:history [(assoc chess/default-game :last-move nil)]
   :selection nil
   :gameover false
   :message "hello world"})
