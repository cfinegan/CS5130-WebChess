(ns chess.cl.events
  (:require
   [re-frame.core :as re-frame]
   [chess.cl.subs :as subs]
   [chess.cl.db :as db]
   [chess.chess :as chess]))

(defn write-json-str [obj]
  (.stringify js/JSON (clj->js obj)))

;; structure of messages received by client:
;;
;; Server starts a game
;; :type :new-game
;; :team :white or :black
;;
;; Server is looking for an opponent
;; :type :finding-game
;;
;; Server rejected request for game 
;; :type :bad-find-game
;;
;; Winner by checkmate
;; :type :winner-checkmate
;;
;; Winner by other means
;; :type :winner
;;
;; Loser by checkmate
;; :type :loser-checkmate
;;
;; Loser by other means
;; :type :loser
;;
;; Server accepted the player's move (selected piece was also valid)
;; :type :valid-move
;;
;; Server accepted the opponent's move (it's your turn now)
;; :type :opponent-moved
;; :from {:x <int> :y <int>}
;; :to {:x <int> :y <int>}
;; :captured {:x <int> :y <int>} or nil
;;
;; Server says that the move does not conform to the rules of the game
;; :type :invalid-move
;;
;; Cannot select empty space
;; :type :no-piece
;;
;; Cannot select opponent's piece
;; :type :wrong-piece
;;
;; Server rejected the move request (invalid game state)
;; :type :bad-move-request
;;
;; Server acknowleged the undo request (client must now wait for opponent)
;; :type :undo-request
;;
;; Server rejected the undo request
;; :type :bad-undo-request
;;
;; Opponent's response to the undo request
;; :type :undo-response
;; :accept? true or false
;;
;; Bad undo response (invalid game state, already processing undo, etc)
;; :type :bad-undo-response
;;
;; Player forfeited (you lose)
;; :type :loser-forfeit
;;
;; Opponent forfeited (you win)
;; :type :winner-forfeit
;;
;; Bad forfeit request (invalid game state)
;; :type :bad-forfeit-request
;;
;; Opponent left the game (after the game was over)
;; :type :leave
;;
;; Bad leave request (invalid game state)
;; :type :bad-leave-request
;;
;; Opponent closed their connection
;; :type :disconnect

(defn read-json-response [r]
  (js->clj (.parse js/JSON (.-data r)) :keywordize-keys true))

(defn client-handle-response [r]
  (let [msg (read-json-response r)
        type (keyword (:type msg))]
    (cond
      (= type :new-game) (re-frame/dispatch [:start-game msg])
      (= type :finding-game) (re-frame/dispatch [:server-finding-game])
      (= type :bad-find-game) (re-frame/dispatch [:bad-find-game])
      (= type :winner-checkmate) (re-frame/dispatch [:winner :checkmate])
      (= type :winner) (re-frame/dispatch [:winner nil])
      (= type :loser-checkmate) (re-frame/dispatch [:loser :checkmate])
      (= type :loser) (re-frame/dispatch [:loser nil])
      (= type :valid-move) (re-frame/dispatch [:valid-move])
      (= type :opponent-moved) (re-frame/dispatch [:opponent-moved msg])
      (= type :invalid-move) (re-frame/dispatch [:invalid-move nil])
      (= type :no-piece) (re-frame/dispatch [:invalid-move :no-piece])
      (= type :wrong-piece) (re-frame/dispatch [:invalid-move :wrong-piece])
      (= type :bad-move-request) (re-frame/dispatch [:bad-move-request])
      (= type :undo-request) (re-frame/dispatch [:undo-respond])
      (= type :bad-undo-request) (re-frame/dispatch [:bad-undo-request])
      (= type :undo-response) (re-frame/dispatch [:undo-handle-response msg])
      (= type :bad-undo-response) (re-frame/dispatch [:bad-undo-response])
      (= type :loser-forfeit) (re-frame/dispatch [:loser :forfeit])
      (= type :winner-forfeit) (re-frame/dispatch [:winner :forfeit])
      (= type :bad-forfeit-request) (re-frame/dispatch [:bad-forfeit-request])
      (= type :leave) (re-frame/dispatch [:winner :leave])
      (= type :bad-leave-request) (re-frame/dispatch [:bad-leave-request])
      (= type :disconnect) (re-frame/dispatch [:winner :disconnect]))))

(set! (.-onmessage db/conn) client-handle-response)

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-fx
 :bad-leave-request
 (fn [cofx _]
   {:db (:db cofx)}))

(re-frame/reg-event-fx
 :bad-forfeit-request
 (fn [cofx _]
   {:db (:db cofx)}))

(re-frame/reg-event-fx
 :bad-undo-response
 (fn [cofx _]
   {:db (:db cofx)}))

(re-frame/reg-event-fx
 :undo-handle-response
 (fn [cofx [_ msg]]
   {:db (:db cofx)}))

(re-frame/reg-event-fx
 :bad-undo-request
 (fn [cofx _]
   {:db (:db cofx)}))

(re-frame/reg-event-fx
 :undo-respond
 (fn [cofx _]
   {:db (:db cofx)}))

(re-frame/reg-event-fx
 :bad-move-request
 (fn [cofx _]
   {:db (:db cofx)}))

(re-frame/reg-event-fx
 :invalid-move
 (fn [cofx [_ kind]]
   (let [game (re-frame/subscribe [::subs/game])]
     {:db
      (assoc
       (:db cofx)
       :game (db/ChessGame.
              (:team @game)
              (pop (:history @game))
              (:rules @game)
              false
              false
              false)
       :selection nil
       :finding-game? false
       :message "invalid move, try again")})))

(re-frame/reg-event-fx
 :opponent-moved
 (fn [cofx [_ msg]]
   (let [game (re-frame/subscribe [::subs/game])
         history (:history @game)
         cur-game (last history)
         from-x (:x (:from msg))
         from-y (:y (:from msg))
         to-x (:x (:to msg))
         to-y (:y (:to msg))]
     (if (and from-x
              from-y
              to-x
              to-y)
       (let [from (chess/->Coord from-x from-y)
             to (chess/->Coord to-x to-y)
             move (chess/->Move from to)]
         {:db
          (assoc
           (:db cofx)
           :game (db/ChessGame.
                  (:team @game)
                  (conj
                   history
                   (assoc
                    (chess/apply-move
                     cur-game
                     move)
                    :last-move move))
                  (:rules @game)
                  false
                  false
                  false)
           :selection nil      
           :finding-game? false
           :message "opponent moved, your turn")})
       (throw (js/Error. "Couldn't read move from opponent."))))))

(re-frame/reg-event-fx
 :valid-move
 (fn [cofx _]
   (let [game (re-frame/subscribe [::subs/game])]
     {:db
      (assoc
       (:db cofx)
       :message (str
                 "the move is valid, waiting for "
                 (chess/team->string
                  (chess/other-team (:team @game)))
                 " to move"))})))

(re-frame/reg-event-fx
 :loser
 (fn [cofx [_ kind]]
   {:db
    (assoc
     (:db cofx)
     :message "you lose")}))

(re-frame/reg-event-fx
 :winner
 (fn [cofx [_ kind]]
   {:db
    (assoc
     (:db cofx)
     :message "you win")}))

(re-frame/reg-event-fx
 :bad-find-game
 (fn [cofx _]
   {:db
    (assoc
     (:db cofx)
     :message "couldn't find game")}))

(re-frame/reg-event-fx
 :server-finding-game
 (fn [cofx _]
   {:db
    (assoc
     (:db cofx) 
     :message "server is looking for an opponent")}))

(re-frame/reg-event-fx
 :start-game
 (fn [cofx [_ msg]]
   (let [game (re-frame/subscribe [::subs/game])
         finding-game? (re-frame/subscribe [::subs/finding-game?])
         team (keyword (:team msg))]
     (if @game
       (throw (js/Error. "Cannot start new game during an active game."))
       (if-not @finding-game?
         (throw (js/Error. "Cannot start new game without a request."))
         (if (or (= team chess/WHITE)
                 (= team chess/BLACK))
           {:db
            (assoc
             (:db cofx)
             :game (db/ChessGame.
                    team
                    [(assoc chess/default-game :last-move nil)]
                    (chess/Rules. true true)
                    false
                    false
                    false)
             :finding-game? false
             :selection nil
             :message (str
                       "game started, you are the "
                       (chess/team->string team)
                       " player"))}
           (throw (js/Error. "New game with invalid team."))))))))
                    

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
             {:type :find-game
              :rules
              {:self-check? true
               :en-passant? true}}))
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
              (write-json-str {:type :undo-request}))
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
                   {:type :move
                    :from {:x (:x selection) :y (:y selection)}
                    :to {:x (:x view-pos) :y (:y view-pos)}}))
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
                          (:rules @game)
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
