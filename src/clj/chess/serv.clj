(ns chess.serv
  (:require [chess.chess :as chess]
            [clojure.data.json :as json])
  (:use org.httpkit.server
        clojure.set
        [clojure.tools.logging :only [info]]
        (compojure [core :only [defroutes GET]])))

;; structure of messages received by server:
;;
;; 1. Find a game
;; :type :find-game
;;   :desired-rules {...}
;;
;; 2. Make a move
;; :type :move
;;   :from {:x <int> :y <int>} :to {:x <int> :y <int>}
;;
;; 3. Request an undo
;; :type :undo-request
;;
;; 4. Accept an undo
;; :type :undo-accept
;;
;; 5. Forfeit the game
;; :type :forfeit
;;
;; 6. Return to lobby
;; :type :return

;; id is the game's unique ID
;; white and black are the two clients
(defrecord ChessGame [id
                      white
                      black
                      history
                      active-team
                      game-over?
                      undo])

;; clients is the set of all currently connected clients
;; client-games is a mapping from clients to game IDs (only ONE GAME PER CLIENT)
;; client-lobby is a mapping from desired rules to a set of clients
;; games is a mapping from game IDs to ChessGames
(defrecord ChessServer [clients
                        client-games
                        client-lobbies
                        games])

;; monotonically increasing game IDs
(let [max-game-id (atom 0)]
  (defn next-game-id []
    (swap! max-game-id inc)))

;; atomic server object
(defonce server (volatile! (ChessServer. #{} nil nil nil)))

;; add this client to the set
(defn server-add-client* [srv channel]
  (do
    (ChessServer.
     (union (:clients srv) #{channel})
     (:client-games srv)
     (:client-lobby srv)
     (:games srv))))

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
      white
      (:black game))))

;; logic for handling matchmaking
(defn server-handle-find-game* [srv channel msg]
  (let [client-games (:client-games srv)
        games (:games srv)]
    ;; client can only look for a game if they're not already playing one
    (if-not (client-games channel)
      (let [lobbies (:client-lobbies srv)
            desired-rules (:desired-rules msg)
            desired-lobby (lobbies desired-rules)]
        (if (and desired-lobby (not (empty? desired-lobby)))
          ;; pick the first player and make a new game
          (let [opponent (first desired-lobby)
                new-desired-lobby (disj desired-lobby opponent)
                new-game (game-create opponent channel)
                new-game-id (:id new-game)
                new-srv (ChessServer.
                         (:clients srv)
                         (-> client-games
                             (assoc channel new-game-id)
                             (assoc opponent new-game-id))
                         (assoc lobbies desired-rules new-desired-lobby)
                         (assoc games new-game-id new-game))
                reply-msg (json/write-str {:type :new-game})]
            (do
              (send! channel reply-msg)
              (send! opponent reply-msg)
              new-srv))
          ;; add them to the set of players looking for the game
          (let [new-desired-lobby (union desired-lobby #{channel})
                new-srv (ChessServer.
                         (:clients srv)
                         client-games
                         (assoc lobbies desired-rules new-desired-lobby)
                         games)]
            (do
              (send! channel (json/write-str {:type :finding-game}))
              new-srv))))
      (do
        (send! channel (json/write-str {:type :bad-find-game}))
        srv))))

(defn server-handle-find-game [channel msg]
  (info channel "is looking for a game")
  (locking server
    (vswap! server server-handle-find-game* channel msg)))

;; TODO: copy the logic from '../../cljs/chess/events.cljs'
(defn server-handle-move* [srv channel msg]
  srv)

(defn server-handle-move [channel msg]
  (info channel "sent a move")
  (locking server
    (vswap! server server-handle-move* channel msg)))

(defn game-request-undo [game channel]
  (let [history (:history game)]
    (if (and history
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
        opponent (and game (game-get-opponent game channel))]
    (if (and opponent (not (:undo game)))
      (let [new-game (game-request-undo game channel)]
        (if new-game
          (let [new-srv (ChessServer.
                         (:clients srv)
                         client-games
                         (:client-lobbies srv)
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
         (:game-over? game)
         nil))
      nil)))

;; client accepts the opponent's request for undo
(defn server-handle-undo-accept* [srv channel msg]
  (let [client-games (:client-games srv)
        games (:games srv)
        game-id (client-games channel)
        game (and game-id (games game-id))
        opponent (and game (game-get-opponent game channel))
        undo (and game (:undo game))]
    (if (and opponent
             undo
             (= undo opponent))
      (let [new-game (game-accept-undo game)]
        (if new-game
          (let [new-srv (ChessServer.
                         (:clients srv)
                         client-games
                         (:client-lobbies srv)
                         (assoc games game-id new-game))]
            (do
              (send! opponent (json/write-str {:type :undo-accept}))
              new-srv))
          (do
            (send! channel (json/write-str {:type :bad-undo-accept}))
            srv)))
      (do
        (send! channel (json/write-str {:type :bad-undo-accept}))
        srv))))

(defn server-handle-undo-accept [channel msg]
  (info channel "accepted an undo")
  (locking server
    (vswap! server server-handle-undo-accept* channel msg)))

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
(defn server-handle-return* [srv channel msg]
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
          (send! opponent (json/write-str {:type :disconnect}))
          new-srv))
      (do
        (send! channel (json/write-str {:type :bad-return-request}))
        srv))))

(defn server-handle-return [channel msg]
  (info channel "is returning to lobby")
  (locking server
    (vswap! server server-handle-return* channel msg)))

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
      (= type :undo-accept) (server-handle-undo-accept channel msg)
      (= type :forfeit) (server-handle-forfeit channel msg)
      (= type :return) (server-handle-return channel msg)
      :else (info channel "sent invalid message"))))

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
