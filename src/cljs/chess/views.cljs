(ns chess.views
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]
   [chess.macros :refer-macros [forv]]
   ))

(defn main-panel []
  [:div "Hello world!"])
