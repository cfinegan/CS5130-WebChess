(ns chess.views
  (:require
   [re-frame.core :as re-frame]
   [chess.subs :as subs]
   [chess.lobby.views :as lobby-views]
   [chess.chess :as chess]
   [chess.macros :refer-macros [forv]]
   ))

(defn main-panel []
  (let [page @(re-frame/subscribe [::subs/page])]
    [:div {:class "container mt-3  p-4 rounded"
           :style {:background-color "#eee"}}
     (cond (= page :lobby) (lobby-views/main-panel)
           :else (throw (js/Error. (str "Invalid page: " page))))]))
