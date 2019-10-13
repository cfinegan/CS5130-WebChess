(ns chess.chess
  (:require #?(:cljs [cljs.core.match :refer-macros [match]]
               :clj [clojure.core.match :refer [match]])
            #?(:clj [clojure.math.numeric-tower :as math])
            [clojure.math.combinatorics :as combo]))

(defn abs [n]
  #?(:cljs (.abs js/Math n)
     :clj (math/abs n)))

;; A Coord is a position on the board
(defrecord Coord [x y])

;; A Piece can be on the board, or be captured
(defrecord Piece [team type moved?])

;; The board is a hash table mapping coords to pieces
;; Ex. (board (Coord 0 0)) => (Piece :rook :black false)

;; 'board' is the current board state. 'white-caps' and 'black-caps'
;; map from a piece type to a count of captures.
;; TODO: Maybe change this to one map that maps from team/type to
;; capture count?
(defrecord GameState [board white-caps black-caps])

(defrecord Move [from to])

(def WHITE :white)
(def BLACK :black)

(def PAWN :pawn)
(def ROOK :rook)
(def BISHOP :bishop)
(def KNIGHT :knight)
(def KING :king)
(def QUEEN :queen)

(defn valid-coord? [coord]
  (and (< (:x coord) 8) (>= (:x coord) 0)
       (< (:y coord) 8) (>= (:y coord) 0)))

(defn accum-tiles [x-xform y-xform board team start]
  (def start-x (x-xform (:x start)))
  (def start-y (y-xform (:y start)))
  (loop [pos (Coord. start-x start-y)
         out '()]
    (def piece (board pos))
    (cond
      ;; Terminate if pos is off board, or has same team's piece.
      (or (not (valid-coord? pos))
          (and piece (= (:team piece) team)))
      out
      ;; Terminate with current tile if pos has other team's piece.
      piece
      (cons pos out)
      ;; Otherwise keep searching
      :else
      (let [new-x (x-xform (:x pos))
            new-y (y-xform (:y pos))]
        (recur (Coord. new-x new-y) (cons pos out))))))

(defn add1 [x] (+ x 1))
(defn sub1 [x] (- x 1))
(defn stay [x] x)

(defn accum-diagonal [board team start]
  (concat
   (accum-tiles add1 add1 board team start)
   (accum-tiles add1 sub1 board team start)
   (accum-tiles sub1 add1 board team start)
   (accum-tiles sub1 sub1 board team start)))

(defn accum-straight [board team start]
  (concat
   (accum-tiles add1 stay board team start)
   (accum-tiles sub1 stay board team start)
   (accum-tiles stay add1 board team start)
   (accum-tiles stay sub1 board team start)))

(defn list->Coord [lst]
  (Coord. (first lst) (second lst)))
(defn abs-not-equal? [coord]
  (not (= (abs (:x coord)) (abs (:y coord)))))
(defn not-zero? [coord]
  (not (and (= 0 (:x coord)) (= 0 (:y coord)))))

(def knight-moves
  (filter abs-not-equal? (map list->Coord (combo/selections [1 2 -1 -2] 2))))

(def king-moves
  (filter not-zero? (map list->Coord (combo/selections [1 0 -1] 2))))

;; TODO: Right now, this assumes that a piece actually exists
;; at 'coord'. We should probably return some bottom value
;; when the piece doesn't exist, distinct from '() since '()
;; is also returned when the piece exists but has no valid moves.
(defn valid-moves [board coord]
  (def px (:x coord))
  (def py (:y coord))
  (def piece (board coord))
  (def team (:team piece))
  (def type (:type piece))
  (def moved? (:moved? piece))
  (defn offset [base]
    (Coord. (+ px (:x base)) (+ py (:y base))))
  (def moves
    (match [type]
      [PAWN]
      (do (def inc (if (= team WHITE) -1 1))
          (def fwd1
            (let [c (Coord. px (+ inc py))]
              (if (board c) nil (list c))))
          (def fwd2
            (and (not moved?) fwd1 (let [c (Coord. px (+ inc inc py))]
                                     (if (board c) nil (list c)))))
          (def ldiag
            (let [c (Coord. (- px 1) (+ inc py))]
              (let [p (board c)]
                (and p (not (= (:team p) team)) (list c)))))
          (def rdiag
            (let [c (Coord. (+ px 1) (+ inc py))]
              (let [p (board c)]
                (and p (not (= (:team p) team)) (list c)))))
          (concat fwd1 fwd2 ldiag rdiag))
      [ROOK]
      (accum-straight board team coord)
      [BISHOP]
      (accum-diagonal board team coord)
      [KNIGHT]
      (map offset knight-moves)
      [KING]
      ;; TODO: Logic for castling goes here? Or somewhere else?
      (map offset king-moves)
      [QUEEN]
      (concat (accum-straight board team coord)
              (accum-diagonal board team coord))
      ))
  (filter valid-coord? moves))

(defn apply-move [game move]
  (def board (:board game))
  (def white-caps (:white-caps game))
  (def black-caps (:black-caps game))
  (def from (:from move))
  (def to (:to move))
  (def move-piece (board from))
  (def cap-piece (board to))
  (def team (:team cap-piece))
  (def new-board (assoc (dissoc board to) from move-piece))
  (def new-white-caps
    (if (= team WHITE)
      white-caps
      (assoc white-caps team (add1 (white-caps team 0)))))
  (def new-black-caps
    (if (= team BLACK)
      black-caps
      (assoc black-caps team (add1 (black-caps team 0)))))
  (GameState. new-board new-white-caps new-black-caps))

(defn winner [game]
  (cond ((:black-caps game) KING) BLACK
        ((:white-caps game) KING) WHITE
        :else nil))

(defn init-game []
  (def wpawn (Piece. WHITE PAWN))
  (def wrook (Piece. WHITE ROOK))
  (def wknight (Piece. WHITE KNIGHT))
  (def wbishop (Piece. WHITE BISHOP))
  (def wqueen (Piece. WHITE QUEEN))
  (def wking (Piece. WHITE KING))
  (def bpawn (Piece. BLACK PAWN))
  (def brook (Piece. BLACK ROOK))
  (def bknight (Piece. BLACK KNIGHT))
  (def bbishop (Piece. BLACK BISHOP))
  (def bqueen (Piece. BLACK QUEEN))
  (def bking (Piece. BLACK KING))
  (def board
    {(Coord. 0 0) brook
     (Coord. 7 0) brook
     (Coord. 1 0) bknight
     (Coord. 6 0) bknight
     (Coord. 2 0) bbishop
     (Coord. 5 0) bbishop
     (Coord. 3 0) bqueen
     (Coord. 4 0) bking
     (Coord. 0 1) bpawn
     (Coord. 1 1) bpawn
     (Coord. 2 1) bpawn
     (Coord. 3 1) bpawn
     (Coord. 4 1) bpawn
     (Coord. 5 1) bpawn
     (Coord. 6 1) bpawn
     (Coord. 7 1) bpawn
     (Coord, 0 7) wrook
     (Coord. 7 7) wrook
     (Coord. 1 7) wknight
     (Coord. 6 7) wknight
     (Coord. 2 7) wbishop
     (Coord. 5 7) wbishop
     (Coord. 3 7) wqueen
     (Coord. 4 7) wking
     (Coord. 0 6) wpawn
     (Coord. 1 6) wpawn
     (Coord. 2 6) wpawn
     (Coord. 3 6) wpawn
     (Coord. 4 6) wpawn
     (Coord. 5 6) wpawn
     (Coord. 6 6) wpawn
     (Coord. 7 6) wpawn})
  (GameState. board {} {}))
