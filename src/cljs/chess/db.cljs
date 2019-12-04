(ns chess.db
  (:require
   [chess.chess :as chess]
   ))

(def conn (js/WebSocket. "ws://127.0.0.1:8080/ws"))

(def default-db
  {:page :lobby
   :match nil
   :lobby {:finding-game? false
           :rules (chess/Rules. false false)}})
