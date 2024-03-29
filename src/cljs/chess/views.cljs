(ns chess.views
  (:require
   [re-frame.core :as re-frame]
   [chess.subs :as subs]
   [chess.lobby.views :as lobby-views]
   [chess.match.views :as match-views]
   [chess.chess :as chess]
   ))

(defn main-panel []
  (let [page @(re-frame/subscribe [::subs/page])]
    [:div {:class "container mt-3 p-4 rounded border"
           :style {:background-color "#eee"}}
     (cond (= page :lobby) [lobby-views/main-panel]
           (= page :match) [match-views/main-panel]
           :else (throw (js/Error. (str "Invalid page: " page))))]))
