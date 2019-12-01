(ns chess.serv
  (:require [chess.chess :as chess]
            [clojure.data.json :as json])
  (:use org.httpkit.server
        clojure.set
        [clojure.tools.logging :only [info]]
        (compojure [core :only [defroutes GET]])))

;; structure of messages received by server:
;;
;; Find a game
;; :type :find-game
;; :rules {...}
;;
;; Make a move
;; :type :move
;; :from {:x <int> :y <int>}
;; :to {:x <int> :y <int>}
;;
;; Request an undo
;; :type :undo-request
;;
;; Respond to an undo
;; :type :undo-response
;; :accept? true or false
;;
;; Forfeit the game
;; :type :forfeit
;;
;; Return to lobby
;; :type :leave

;; id is the game's unique ID
;; white and black are the two clients
;; TOOD: Storing active-team here isn't necessary, the active team can
;; be inferred from the game history by calling chess/active-team.
(defrecord ChessGame [id white black history active-team game-over? undo])

;; clients is the set of all currently connected clients
;; client-games is a mapping from clients to game IDs (only ONE GAME PER CLIENT)
;; client-lobby is a mapping from desired rules to a set of clients
;; games is a mapping from game IDs to ChessGames
(defrecord ChessServer [clients client-games lobbies games])

;; monotonically increasing game IDs
(let [max-game-id (atom 0)]
  (defn next-game-id []
    (swap! max-game-id inc)))

;; atomic server object
(defonce server (volatile! (ChessServer. #{} nil nil nil)))

;; add this client to the set
(defn server-add-client* [srv channel]
  (ChessServer.
   (union (:clients srv) #{channel})
   (:client-games srv)
   (:lobbies srv)
   (:games srv)))

(defn game-create [white black]
  (ChessGame.
   (next-game-id)
   white
   black
   [(assoc chess/default-game :last-move nil)]
   chess/WHITE
   false
   nil))

(defn game-get-opponent [game channel]
  (let [white (:white game)]
    (if (= white channel)
      (:black game)
      white)))

(defn game-get-team [game channel]
  (cond
    (= (:white game) channel) chess/WHITE
    (= (:black game) channel) chess/BLACK
    :else nil))

;; logic for handling matchmaking
(defn server-handle-find-game* [srv channel msg]
  (let [client-games (:client-games srv)
        games (:games srv)]
    ;; client can only look for a game if they're not already playing one
    (if (or (nil? client-games)
            (nil? (client-games channel)))
      (let [lobbies (:lobbies srv)
            rules (:rules msg)
            lobby (and lobbies (lobbies rules))]
        (if (and lobby (not (empty? lobby)))
          ;; pick the first player and make a new game
          (let [opponent (first lobby)
                new-lobby (disj lobby opponent)
                new-game (game-create opponent channel)
                new-game-id (:id new-game)
                new-srv (ChessServer.
                         (:clients srv)
                         (-> client-games
                             (assoc channel new-game-id)
                             (assoc opponent new-game-id))
                         (assoc lobbies rules new-lobby)
                         (assoc games new-game-id new-game))]
            (do
              (send! channel (json/write-str {:type :new-game :team :black}))
              (send! opponent (json/write-str {:type :new-game :team :white}))
              (info "new game for" channel "and" opponent)
              new-srv))
          ;; add them to the set of players looking for the game
          (let [new-lobby (union lobby #{channel})
                new-srv (ChessServer.
                         (:clients srv)
                         client-games
                         (assoc lobbies rules new-lobby)
                         games)]
            (do
              ;; (send! channel (json/write-str {:type :finding-game}))
              new-srv))))
      (do
        (send! channel (json/write-str {:type :bad-find-game}))
        srv))))

(defn server-handle-find-game [channel msg]
  (info channel "is looking for a game with rules" (:rules msg))
  (locking server
    (vswap! server server-handle-find-game* channel msg)))

(defn server-handle-move* [srv channel msg]
  (let [client-games (:client-games srv)
        games (:games srv)
        game-id (client-games channel)
        game (and game-id (games game-id))
        opponent (and game (game-get-opponent game channel))
        team (and game (game-get-team game channel))
        from-msg (:from msg)
        to-msg (:to msg)
        from (and from-msg
                  (:x from-msg)
                  (:y from-msg)
                  (chess/->Coord (:x from-msg) (:y from-msg)))
        to (and to-msg
                (:x to-msg)
                (:y to-msg)
                (chess/->Coord (:x to-msg) (:y to-msg)))]
    (if (and opponent
             team
             (not (:undo game))
             (= (:active-team game) team)
             from
             to)
      (let [cur-history (:history game)
            cur-game (last cur-history)            
            cur-board (:board cur-game)
            cur-caps (:captures game)
            piece (cur-board from)]
        (if (and piece (= (:team piece) team))
          (let [moves (chess/valid-moves cur-history from true)]
            (if (some #(= to %) moves)
              (let [move (chess/->Move from to)
                    new-game (assoc
                              (chess/apply-move cur-game move)
                              :last-move move)
                    new-history (conj cur-history new-game)
                    winner (chess/winner (:captures new-game))
                    check? (chess/check?
                            (chess/other-team team)
                            new-history)
                    checkmate? (chess/check-mate?
                                (chess/other-team team)
                                new-history)
                    update-srv (fn [g]
                                 (ChessServer.
                                  (:clients srv)
                                  client-games
                                  (:lobbies srv)
                                  (assoc games game-id g)))
                    update-game (fn [o]
                                  (ChessGame.
                                   (:id game)
                                   (:white game)
                                   (:black game)
                                   new-history
                                   (chess/other-team team)
                                   o
                                   nil))
                    over? (or (= winner team) checkmate?)]
                (do
                  (when over?
                    (send!
                     channel
                     (json/write-str
                      {:type :game-over})))
                  (send!
                   opponent
                   (json/write-str
                    {:type :opponent-moved
                     :from {:x (:x from) :y (:y from)}
                     :to {:x (:x to) :y (:y to)}
                     :check? check?
                     :game-over? over?}))
                  (update-srv (update-game over?))))
              (do
                (send! channel (json/write-str {:type :invalid-move}))
                srv)))
          (do
            (send!
             channel
             (if piece
               (json/write-str {:type :wrong-piece})
               (json/write-str {:type :no-piece})))
            srv)))
      (do
        (send! channel (json/write-str {:type :bad-move-request}))
        srv))))

(defn server-handle-move [channel msg]
  (info channel "sent a move")
  (locking server
    (vswap! server server-handle-move* channel msg)))

(defn game-request-undo [game channel]
  (let [history (:history game)]
    (if (and (not (:game-over? game))
             history
             (> (count history) 1))
      (ChessGame.
       (:id game)
       (:white game)
       (:black game)
       (:history game)
       (:active-team game)
       (:game-over? game)
       channel)
      nil)))

;; update the state to remember that this client requested an undo
(defn server-handle-undo-request* [srv channel msg]
  (let [client-games (:client-games srv)
        games (:games srv)
        game-id (client-games channel)
        game (and game-id (games game-id))
        team (and game (game-get-team game channel))
        opponent (and game (game-get-opponent game channel))]
    (if (and opponent
             team
             (not (:undo game))
             (not (= (:active-team game) team)))
      (let [new-game (game-request-undo game channel)]
        (if new-game
          (let [new-srv (ChessServer.
                         (:clients srv)
                         client-games
                         (:lobbies srv)
                         (assoc games game-id new-game))]
            (do
              (send! opponent (json/write-str {:type :undo-request}))
              new-srv))
          (do
            (send! channel (json/write-str {:type :bad-undo-request}))
            srv)))
      (do
        (send! channel (json/write-str {:type :bad-undo-request}))
        srv))))

(defn server-handle-undo-request [channel msg]
  (info channel "requested an undo")
  (locking server
    (vswap! server server-handle-undo-request* channel msg)))

(defn game-accept-undo [game]
  (let [history (:history game)]
    (if (and history
             (> (count history) 1))
      (let [white (:white game)
            undo (:undo game)]
        (ChessGame.
         (:id game)
         white
         (:black game)
         (pop history)
         (if undo
           (if (= undo white) chess/WHITE chess/BLACK)
           (:active-team game))
         false
         nil))
      nil)))

;; client accepts the opponent's request for undo
(defn server-handle-undo-response* [srv channel msg]
  (let [client-games (:client-games srv)
        games (:games srv)
        game-id (client-games channel)
        game (and game-id (games game-id))
        opponent (and game (game-get-opponent game channel))
        undo (and game (:undo game))
        accept? (:accept? msg)]
    (if (and (not (nil? accept?))
             opponent
             undo
             (= undo opponent))
      (let [new-game (game-accept-undo game)]
        (if new-game
          (let [new-srv (ChessServer.
                         (:clients srv)
                         client-games
                         (:lobbies srv)
                         (assoc games game-id new-game))]
            (do
              (send!
               opponent
               (json/write-str
                {:type :undo-response
                 :accept? accept?}))
              new-srv))
          (do
            (send! channel (json/write-str {:type :bad-undo-response}))
            srv)))
      (do
        (send! channel (json/write-str {:type :bad-undo-response}))
        srv))))

(defn server-handle-undo-response [channel msg]
  (info channel "responded to an undo request")
  (locking server
    (vswap! server server-handle-undo-response* channel msg)))

;; forfeit is possible at any time
(defn server-handle-forfeit* [srv channel msg]
  (let [client-games (:client-games srv)
        games (:games srv)
        game-id (client-games channel)
        game (and game-id (games game-id))
        opponent (and game (game-get-opponent game channel))]
    (if (and opponent
             (not (:game-over? game))
             (not (:undo game)))
      (let [new-game (ChessGame.
                      (:id game)
                      (:white game)
                      (:black game)
                      (:history game)
                      nil
                      true
                      nil)
            new-srv (ChessServer.
                     (:clients srv)
                     client-games
                     (:client-lobbies srv)
                     (assoc games game-id new-game))]
        (do
          (send! channel (json/write-str {:type :loser-forfeit}))
          (send! opponent (json/write-str {:type :winner-forfeit}))
          new-srv))
      (do
        (send! channel (json/write-str {:type :bad-forfeit-request}))
        srv))))

(defn server-handle-forfeit [channel msg]
  (info channel "is forfeiting")
  (locking server
    (vswap! server server-handle-forfeit* channel msg)))

;; return to lobby should only be possible once the game has ended
(defn server-handle-leave* [srv channel msg]
  (let [client-games (:client-games srv)
        games (:games srv)
        game-id (client-games channel)
        game (and game-id (games game-id))]
    (if (and game
             (:game-over? game))
      (let [white (if (= channel (:white game)) 
                    nil (:white game))
            black (if (= channel (:black game))
                    nil (:black game))
            new-game (ChessGame.
                      (:id game)
                      white
                      black
                      (:history game)
                      nil
                      true
                      nil)
            new-srv (ChessServer.
                     (:clients srv)
                     (dissoc client-games channel)
                     (:client-lobbies srv)
                     (assoc games game-id new-game))
            opponent (if white white black)]
        (do
          (send! opponent (json/write-str {:type :leave}))
          new-srv))
      (do
        (send! channel (json/write-str {:type :bad-leave-request}))
        srv))))

(defn server-handle-leave [channel msg]
  (info channel "is returning to lobby")
  (locking server
    (vswap! server server-handle-leave* channel msg)))

(defn value-reader [key value]
  (cond
    (= key :type) (keyword value)
    :else value))

;; received a message from a client
(defn server-handle-message [channel data]
  (let [msg (json/read-str data :key-fn keyword :value-fn value-reader)
        type (:type msg)]
    (cond
      (= type :find-game) (server-handle-find-game channel msg)
      (= type :move) (server-handle-move channel msg)
      (= type :undo-request) (server-handle-undo-request channel msg)
      (= type :undo-response) (server-handle-undo-response channel msg)
      (= type :forfeit) (server-handle-forfeit channel msg)
      (= type :leave) (server-handle-leave channel msg)
      :else (info channel "sent invalid message" msg))))

;; a client disconnected, so end any games they were playing
(defn server-game-end-disconnect* [srv channel]
  (let [games (:games srv)
        client-games (:client-games srv)
        game-id (and client-games (client-games channel))
        game (and game-id (games game-id))]
    (if game
      (let [white (if (= channel (:white game))
                    nil (:white game))
            black (if (= channel (:black game))
                    nil (:black game))
            other-client (if white white black)]
        (if other-client
          ;; game is over, but don't remove it
          ;; unless both clients have disconnected
          (let [new-game (ChessGame.
                          (:id game)
                          white
                          black
                          (:history game)
                          nil
                          true
                          nil)]
            (do
              (send! other-client (json/write-str {:type :disconnect}))
              (-> games
                  (dissoc game-id)
                  (assoc game-id new-game))))
          (dissoc games game-id)))
      games)))

;; remove this client from the set
(defn server-close-channel* [srv channel]
  (ChessServer.
   (disj (:clients srv) channel)
   (dissoc (:client-games srv) channel)
   (dissoc (:client-lobby srv) channel)
   (server-game-end-disconnect* srv channel)))

;; handler for when a client closes their connection to the server
(defn server-close-channel [channel status]
  (info channel "closed, status" status)
  (locking server
    (vswap! server server-close-channel* channel)))

;; request handler
(defn app [req]
  (with-channel req channel
    (info channel "connected")
    (locking server
      (vswap! server server-add-client* channel))
    (on-receive channel #(server-handle-message channel %))
    (on-close channel #(server-close-channel channel %))))

;; this is a function that stops the server (returned by run-server)
(defonce server-fn (atom nil))

;; clean up everything here
(defn stop-server []
  (when-not (nil? @server-fn)
    (@server-fn :timeout 100)
    (reset! server-fn nil)))

(defroutes routes
  (GET "/ws" [] app))

;; entry point
(defn -main [& args]
  (reset! server-fn (run-server #'routes {:port 8080}))
  (info "server started on port 8080"))
