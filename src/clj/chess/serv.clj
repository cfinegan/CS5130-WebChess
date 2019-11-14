(ns chess.serv
  (:require [chess.chess :as chess]
            [clojure.data.json :as json])
  (:use org.httpkit.server
        clojure.set
        [clojure.tools.logging :only [info]]))

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
                      game-over])

;; clients is the set of all currently connected clients
;; client-games is a mapping from clients to game IDs
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
(defonce server (atom (ChessServer. #{} nil nil nil)))

;; add this client to the set
(defn server-add-client* [srv channel]
  (ChessServer.
   (union (:clients srv) #{channel})
   (:client-games srv)
   (:client-lobby srv)
   (:games srv)))

(defn create-game [white black]
  (ChessGame.
   (next-game-id)
   white
   black
   [(assoc chess/default-game :last-move nil)]
   chess/WHITE
   false))

;; logic for handling matchmaking
(defn server-handle-find-game* [srv channel msg]
  (let [client-games (:client-games srv)
        games (:games srv)]
    ;; client can only look for a game if they're not already playing one
    (if (not (client-games channel))
      (let [lobbies (:client-lobbies srv)
            desired-rules (:desired-rules msg)
            desired-lobby (lobbies desired-rules)]
        (if (and desired-lobby
                 (not (empty? desired-lobby)))
          ;; pick the first player and make a new game
          (let [opponent (first desired-lobby)
                new-desired-lobby (disj desired-lobby opponent)
                new-game (create-game opponent channel)
                new-game-id (:id new-game)
                new-srv (ChessServer.
                         (:clients srv)
                         (-> client-games
                             (assoc channel new-game-id)
                             (assoc opponent new-game-id))
                         (assoc lobbies desired-rules new-desired-lobby)
                         (assoc games new-game-id new-game))]
            new-srv)
          ;; add them to the set of players looking for the game
          (let [new-desired-lobby (union desired-lobby #{channel})
                new-srv (ChessServer.
                         (:clients srv)
                         client-games
                         (assoc lobbies desired-rules new-desired-lobby)
                         games)]
            new-srv)))
      srv)))

;; we reply to the clients after we do the swap
(defn server-handle-find-game [channel msg]
  (info channel "is looking for a game")
  (swap! server server-handle-find-game* channel msg))

;; TODO: copy the logic from '../../cljs/chess/events.cljs'
(defn server-handle-move* [srv channel msg]
  srv)

(defn server-handle-move [channel msg]
  (info channel "sent a move")
  (swap! server server-handle-move* channel msg))

;; update the state to remember that this client requested an undo
(defn server-handle-undo-request* [srv channel msg]
  ;; TODO
  srv)

(defn server-handle-undo-request [channel msg]
  (info channel "requested an undo")
  (swap! server server-handle-undo-request* channel msg))

;; client accepts the opponent's request for undo
(defn server-handle-undo-accept* [srv channel msg]
  ;; TODO
  srv)

(defn server-handle-undo-accept [channel msg]
  (info channel "accepted an undo")
  (swap! server server-handle-undo-accept* channel msg))

;; forfeit is possible at any time
(defn server-handle-forfeit* [srv channel msg]
  ;; TODO
  srv)

(defn server-handle-forfeit [channel msg]
  (info channel "is forfeiting")
  (swap! server server-handle-forfeit* channel msg))

;; return to lobby should only be possible once the game has ended
(defn server-handle-return* [srv channel msg]
  ;; TODO
  srv)

(defn server-handle-return [channel msg]
  (info channel "is returning to lobby")
  (swap! server server-handle-return* channel msg))

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
                          true)]
            ;; send new-game to the other client
            ;; along with another message that the 
            ;; other player has disconnected
            (-> games
                (dissoc game-id)
                (assoc game-id new-game)))
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
  (swap! server server-close-channel* channel)
  (info channel "closed, status" status))

;; request handler
(defn app [req]
  (with-channel req channel
    (info channel "connected")
    (swap! server server-add-client* channel)
    (on-receive channel #(server-handle-message channel %))
    (on-close channel #(server-close-channel channel %))))

;; this is a function that stops the server (returned by run-server)
(defonce server-fn (atom nil))

;; clean up everything here
(defn stop-server []
  (when-not (nil? @server-fn)
    (@server-fn :timeout 100)
    (reset! server-fn nil)))

;; entry point
(defn -main [& args]
  (reset! server-fn (run-server app {:port 8080}))
  (info "server started on port 8080"))
