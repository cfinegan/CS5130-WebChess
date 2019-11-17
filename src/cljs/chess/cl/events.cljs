(ns chess.cl.events
  (:require
   [re-frame.core :as re-frame]
   [chess.cl.subs :as subs]
   [chess.cl.db :as db]
   [chess.chess :as chess]))

(defn write-json-str [obj]
  (.stringify js/JSON obj))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-fx
 :find-game-click
 (fn [cofx _]
   (let [game (re-frame/subscribe [::subs/game])
         finding-game? (re-frame/subscribe [::subs/finding-game?])]
     (if @game
       (throw (js/Error. "Find game request while a game is active."))
       (if @finding-game?
         {:db (assoc (:db cofx) :message "currently looking for a game, request denied")}
         (do
           (.send
            db/conn
            (write-json-str
             (clj->js
              {:type :find-game
               :rules
               {:self-check? true
                :en-passant? true}})))
           {:db
            (assoc
             (:db cofx)
             :game nil
             :finding-game? true
             :selection nil
             :message "waiting for a game to join")}))))))

(re-frame/reg-event-fx
 :undo-click
 (fn [cofx _]
   (let [game (re-frame/subscribe [::subs/game])
         active-team (re-frame/subscribe [::subs/active-team])
         my-team (and @game (:team @game))
         history (and @game (:history @game))]
     (if (and @active-team
              my-team
              history)
       (if (not (= @active-team my-team))
         (if (> (count history) 1)
           (do
             (.send
              db/conn
              (write-json-str
               (clj->js {:type :undo-request})))
             {:db
              (assoc
               (:db cofx)
               :game (db/ChessGame.      
                      my-team
                      history
                      (:rules game)
                      (:game-over? game)
                      true
                      false)
               :finding-game? false
               :selection nil
               :message "requested undo, waiting for response")})
           {:db
            (assoc
             (:db cofx)
             :game game
             :finding-game? false
             :selection nil
             :message "cannot request undo in starting position")})
         {:db
          (assoc
           (:db cofx)
           :game game
           :finding-game? false
           :selection nil
           :message "cannot request undo on your turn")})
       (throw (js/Error. "Undo request for invalid game."))))))
           

(re-frame/reg-event-fx
 :board-click
 (fn [cofx [_ x y]]
   (let [game (re-frame/subscribe [::subs/game])]
     (if @game
       (let [sent-move? (:sent-move? @game)
             history (:history @game)
             my-team (:team @game)
             game-over? (:game-over? @game)
             active-team @(re-frame/subscribe [::subs/active-team])
             rotate? (= my-team chess/BLACK)
             view-selection (re-frame/subscribe [::subs/selection])
             selection (and @view-selection
                            (if rotate?
                              (chess/rotate-coord @view-selection)
                              @view-selection))
             current-game (last history)
             board (:board current-game)
             view-board (if rotate? (chess/rotate-board board) board)
             caps (:captures game)
             pos (chess/->Coord x y)
             view-pos (if rotate? (chess/rotate-coord pos) pos)
             piece (view-board pos)]
         (cond
           game-over?
           {:db (:db cofx)}

           sent-move?
           {:db
            (assoc
             (:db cofx)
             :selection nil)}

           ;; we already have a 'whos-turn' panel
           (not (= active-team my-team))
           {:db (:db cofx)}
           
           selection
           (if (= selection view-pos)
             {:db (assoc (:db cofx) :selection nil)}
             (let [move (chess/->Move selection view-pos)]
               (do
                 (.send
                  db/conn
                  (write-json-str
                   (clj->js
                    {:type :move
                     :from {:x selection :y selection}
                     :to {:x view-pos :y view-pos}})))
                 {:db
                  (assoc
                   (:db cofx)
                   :game (db/ChessGame.
                          my-team
                          (conj
                           history
                           (assoc
                            (chess/apply-move
                             current-game
                             move)
                            :last-move move))
                          (:rules game)
                          game-over?
                          false
                          true)
                   :selection nil      
                   :finding-game? false
                   :message "applied move, waiting for validation")})))

           (not piece)
           {:db (assoc (:db cofx) :message "cannot select empty space")}

           (not (= (:team piece) my-team))
           {:db (assoc (:db cofx) :message "cannot select opposite team")}
           
           (not selection)
           {:db
            (assoc
             (:db cofx)
             :selection pos
             :message (str
                       "select a destination"
                       (if (chess/check? game my-team history)
                         " (you are in check)"
                         "")))}

           :else
           {:db (assoc (:db cofx) :message "board-click is in an unknown state")}))
       (throw (js/Error. "Board click for an invalid game."))))))
