(ns chess.db
  (:require [chess.chess :as chess]))

(def default-db
  {:history [chess/default-game]
   :selection nil
   :gameover false
   :message "hello world"})
