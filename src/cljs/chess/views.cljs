(ns chess.views
  (:require
   [re-frame.core :as re-frame]
   [chess.subs :as subs]
   [chess.chess :as chess]
   [chess.macros :refer-macros [forv]]
   ))

(defn piece->unicode [{team :team type :type}]
  (cond (= team chess/WHITE)
        (cond (= type chess/KING) "&#x2654;"
              (= type chess/QUEEN) "&#x2655;"
              (= type chess/ROOK) "&#x2656;"
              (= type chess/BISHOP) "&#x2657;"
              (= type chess/KNIGHT) "&#x2658;"
              (= type chess/PAWN) "&#x2659;")
        (= team chess/BLACK)
        (cond (= type chess/KING) "&#x265A;"
              (= type chess/QUEEN) "&#x265B;"
              (= type chess/ROOK) "&#x265C;"
              (= type chess/BISHOP) "&#x265D;"
              (= type chess/KNIGHT) "&#x265E;"
              (= type chess/PAWN) "&#x265F;")))

(defn make-on-click [x y]
  (fn [e]
    (.preventDefault e)
    (re-frame/dispatch [:board-click x y])))

(defn board-panel []
  (let [game (re-frame/subscribe [::subs/game])
        active-team (re-frame/subscribe [::subs/active-team])
        rotate? (and active-team (= @active-team chess/BLACK))
        board (and game (:board @game))
        view-board (and board
                        (if rotate?
                          (chess/rotate-board board)
                          board))
        caps (and game (:captures @game))
        last-move (re-frame/subscribe [::subs/last-move])
        view-selection (re-frame/subscribe [::subs/selection])
        selection (and @view-selection
                       (if rotate?
                         (chess/rotate-coord @view-selection)
                         @view-selection))
        history (re-frame/subscribe [::subs/history])
        moves (and selection
                   (chess/valid-moves board selection @history true))
        piece-sel (board selection)
        team (and selection (:team (board selection)))
        otherteam (chess/other-team team)
        enemy (and otherteam
                   (chess/find-team board otherteam))
        futures (map
                 #(vector
                   %
                   (chess/apply-move
                    @game
                    (chess/Move. selection %)))
                     moves)
        future-captures (map
                         (fn [f]
                           [(first f)
                            (filter
                             (fn [c]
                               (not (some #(= c %) caps)))
                             (:captures (peek f)))])
                         futures)
        enemy-moves (apply
                     concat
                     (map
                      #(apply
                        concat
                        (map
                         (fn [e]
                           (map
                            (fn [m] [% (chess/Move. e m)])
                            (chess/valid-moves
                             (:board (peek %))
                             e
                             (conj @history (peek %))
                             true)))
                         enemy))
                      futures))
        enemy-futures (map
                       #(vector
                         (first (first %))
                         (peek
                          (chess/apply-move-to-board
                           (:board (peek (first %)))
                           (peek %))))
                       enemy-moves)
        enemy-captures (filter
                        #(and (peek %)
                              (not (empty? (peek %)))
                              (= (:id (peek %))
                                 (:id piece-sel)))
                        enemy-futures)
        vuln-moves (filter 
                    (fn [m] (some #(= m (first %)) enemy-captures))
                    moves)
        capture-moves (filter
                       (fn [m]
                         (some
                          #(and
                            (= m (first %))
                            (not (empty? (peek %))))
                          future-captures))
                       moves)]
    [:div
     [:table {:border 1
              :style  {:table-layout "fixed"
                       :width "350px"
                       :height "300px"
                       :text-align "center"}}
      `[:tbody
        ~@(forv
           [i (range 8)]
           `[:tr
             ~@(forv
                [j (range 8)]
                (let [view-pos (chess/->Coord j i)
                      pos (if rotate? (chess/rotate-coord view-pos) view-pos)
                      piece (board pos)
                      bg (cond (= pos selection) "#f3f781"
                               (and vuln-moves
                                    capture-moves
                                    (some #(= pos %) vuln-moves)
                                    (some #(= pos %) capture-moves)) "#b404ae"
                               (and vuln-moves
                                    (some #(= pos %) vuln-moves)) "#fa5858"
                               (and capture-moves
                                    (some #(= pos %) capture-moves)) "#2efe64"
                               (and moves (some #(= pos %) moves)) "#2eccfa"
                               (and @last-move
                                    (let [r-from (if rotate?
                                                   (chess/rotate-coord
                                                    (:from @last-move))
                                                   (:from @last-move))
                                          r-to (if rotate?
                                                 (chess/rotate-coord
                                                  (:to @last-move))
                                                 (:to @last-move))
                                          {fx :x fy :y} r-from
                                          {tx :x ty :y} r-to]
                                      (or (and (= fx j)
                                               (= fy i))
                                          (and (= tx j)
                                               (= ty i))))) "#cccccc"
                               :else (if (= (mod j 2) 0)
                                       (if (= (mod i 2) 1)
                                         "#ffffff"
                                         "#f7f2f0")
                                       (if (= (mod i 2) 1)
                                         "#f7f2f0"
                                         "#ffffff")))]
                  [:td {:on-click (make-on-click j i)
                        :style {:background-color bg}
                        :dangerouslySetInnerHTML
                        {:__html (if piece
                                   (piece->unicode piece)
                                   "&nbsp;")}}]))])]]]))

(defn message-panel []
  (let [msg (re-frame/subscribe [::subs/message])]
    [:div @msg]))

(defn on-undo-click [e]
  (.preventDefault e)
  (re-frame/dispatch [:undo-click]))

(defn undo-button []
  (let [history (re-frame/subscribe [::subs/history])]
    [:div [:button {:class "btn btn-dark"
                    :on-click on-undo-click
                    :disabled (<= (count @history) 1)} "undo"]]))

(defn whos-turn-panel []
  (let [team @(re-frame/subscribe [::subs/active-team])]
    [:div "it's " (chess/team->string team) "'s turn"]))

(defn main-panel []
  [:div {:class "container mt-3 p-4 rounded"
         :style {:background-color "#eee"}}
   (message-panel)
   [:br]
   (board-panel)
   [:br]
   (undo-button)
   [:br]
   (whos-turn-panel)])
