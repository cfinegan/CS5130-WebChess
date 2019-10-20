(ns chess.db
  (:require [chess.chess :as chess]))

(def default-db
  {:history [chess/default-game]
   :selection nil
   :message "hello world"})
