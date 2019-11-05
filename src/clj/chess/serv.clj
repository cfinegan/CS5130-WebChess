(ns chess.serv
  (:require [chess.chess :as chess]))

(def default-board (:board chess/default-game))

(defn -main [& args]
  (println (chess/check? default-board chess/WHITE [])))
