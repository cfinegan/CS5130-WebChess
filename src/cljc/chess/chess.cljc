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
;; are lists of captured pieces, so we can show which pieces have been
;; captured in our UI. We will keep a list of these GameState objects,
;; so that we can rewind easily.
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
