(ns chess.cl.db
  (:require [chess.chess :as chess]))

(defrecord ChessGame [team history rules game-over? undo? sent-move?])

(def conn (js/WebSocket. "ws://127.0.0.1:8080/ws"))

(def default-db
  {:game nil
   :finding-game? false
   :selection nil
   :message "find a game"})
