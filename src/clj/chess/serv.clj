(ns chess.serv
  (:require [chess.chess :as chess]
            [clojure.data.json :as json])
  (:use org.httpkit.server
        clojure.set
        [clojure.tools.logging :only [info]]))

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
;; games is a mapping from game IDs to ChessGames
(defrecord ChessServer [clients
                        client-games
                        games])

;; monotonically increasing game IDs
(let [max-game-id (atom 0)]
  (defn next-game-id []
    (swap! max-game-id inc)))

;; atomic server object
(defonce server (atom (ChessServer. #{} nil nil)))

;; add this client to the set
(defn server-add-client [srv channel]
  (ChessServer.
   (union (:clients srv) #{channel})
   (:client-games srv)
   (:games srv)))

(defn server-handle-find-game [channel msg]
  (info channel "is looking for a game"))

(defn server-handle-move [channel msg]
  (info channel "sent a move"))

(defn server-handle-undo [channel msg]
  (info channel "requested an undo"))

(defn server-handle-forfeit [channel msg]
  (info channel "is forfeiting"))

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
      (= type :undo) (server-handle-undo channel msg)
      (= type :forfeit) (server-handle-forfeit channel msg)
      :else (info channel "sent invalid message"))))

;; a client disconnected, so end any games they were playing
(defn server-game-end-disconnect [srv channel]
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
          ;; game is over, but don't remove it until
          ;; both clients have disconnected
          (let [new-game (ChessGame. (:id game)
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
(defn server-remove-client [srv channel]
  (ChessServer.
   (disj (:clients srv) channel)
   (dissoc (:client-games srv) channel)
   (server-game-end-disconnect srv channel)))

;; handler for when a client closes their connection to the server
(defn server-close-channel [channel status]
  (swap! server server-remove-client channel)
  (info channel "closed, status" status))

;; request handler
(defn app [req]
  (with-channel req channel
    (info channel "connected")
    (swap! server server-add-client channel)
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
