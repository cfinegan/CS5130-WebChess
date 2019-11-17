(ns chess.test.db
  (:require [chess.chess :as chess]))

(def default-db
  {:history [(assoc chess/default-game :last-move nil)]
   :selection nil
   :gameover false
   :rules (chess/Rules. true true)
   :message "hello world"})
