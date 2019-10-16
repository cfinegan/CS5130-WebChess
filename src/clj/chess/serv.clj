(ns chess.serv
  (:require [chess.chess :as chess]))

(defn -main [& args]
  (println chess/default-game))
