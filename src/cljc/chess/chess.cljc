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

;; 'board' is a hash table mapping coords to pieces
;; Ex. (board (Coord 0 0)) => (Piece :rook :black false)
;; 'captures' is a hash table mapping teams/type pairs to number of captures
;; Ex. (caps (cons WHITE PAWN)) => 2
;;     (caps (cons BLACK KING)) => nil
;; Use 0 as a default value to get an accurate count when none are captured
;;     (caps (cons BLACK KING) 0) => 0
(defrecord GameState [board captures])

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
  (let [start-x (x-xform (:x start))
        start-y (y-xform (:y start))]
    (loop [pos (Coord. start-x start-y)
           out []]
      (let [piece (board pos)]
        (cond
          ;; Terminate if pos is off board, or has same team's piece.
          (or (not (valid-coord? pos))
              (and piece (= (:team piece) team)))
          out
          ;; Terminate with current tile if pos has other team's piece.
          piece
          (conj pos out)
          ;; Otherwise keep searching
          :else
          (let [new-x (x-xform (:x pos))
                new-y (y-xform (:y pos))]
            (recur (Coord. new-x new-y) (conj pos out))))))))

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
(defn valid-moves [board {px :x py :y :as coord}]
  (let [{team :team type :type moved? :moved? :as piece} (board coord)
        offset (fn [{x :x y :y}] (Coord. (+ px x) (+ py y)))]
    (filter
     valid-coord?
     (cond
       (= type PAWN)
       (let [dir (if (= team WHITE) -1 1)
             fwd1 (let [c (Coord. px (+ dir py))]
                    (if (board c) nil (list c)))
             fwd2 (and (not moved?)
                       fwd1
                       (let [c (Coord. px (+ dir dir py))]
                         (if (board c) nil (list c))))
             ldiag (let [c (Coord. (- px 1) (+ dir py))
                         p (board c)]
                     (and p (not (= (:team p) team)) (list c)))
             rdiag (let [c (Coord. (+ px 1) (+ dir py))
                         p (board c)]
                     (and p (not (= (:team p) team)) (list c)))]
         (concat fwd1 fwd2 ldiag rdiag))
       (= type ROOK)
       (accum-straight board team coord)
       (= type BISHOP)
       (accum-diagonal board team coord)
       (= type KNIGHT)
       (map offset knight-moves)
       (= type KING)
       ;; TODO: Logic for castling goes here? Or somewhere else?
       (map offset king-moves)
       (= type QUEEN)
       (concat (accum-straight board team coord)
               (accum-diagonal board team coord))))))

(defn apply-move-to-board [board {to :to from :from}]
  (let [{team :team type :type moved? :moved? :as piece} (board from)
        move-piece (if moved? piece (Piece. team type true))
        cap-piece (board to)]
    [(-> board
         (dissoc from)
         (assoc to move-piece))
     cap-piece]))

(defn apply-move-to-board* [board move]
  (first (apply-move-to-board board move)))

(defn apply-move [{board :board caps :captures} {from :from to :to :as move}]
  (let [[new-board cap-piece] (apply-move-to-board board move)]
    (GameState.
     new-board
     (if cap-piece
       (let [{team :team type :type} cap-piece
             team+type (cons team type)]
         (assoc caps team+type (inc (caps team+type 0))))
       caps))))

(defn winner [game]
  (let [caps (:captures game)]
    (cond (caps (cons BLACK KING)) WHITE
          (caps (cons WHITE KING)) BLACK
          :else nil)))

(defn find-king [board team]
  (loop [board (seq board)]
    (if (empty? board)
      nil
      (let [[pos piece] (first board)]
        (if (and (= team (:team piece))
                 (= KING (:type piece)))
          pos
          (recur (rest board)))))))

(defn find-team [board team]
  (for [[pos piece] board
        :when (= team (:team piece))]
    pos))

(defn other-team [team]
  (cond (= team WHITE) BLACK
        (= team BLACK) WHITE))

(defn check? [board team]
  (let [king (find-king board team)
        enemy (find-team board (other-team team))
        moves (flatten (map #(valid-moves board %1) enemy))]
    (println moves)
    (loop [moves moves]
      (let [{to :to} (first moves)]
        (cond (empty? moves) false
              (= king to) true
              :else (recur (rest moves)))))))

(defn check-mate? [board team]
  (let [friends (find-team board team)
        moves (flatten (map valid-moves friends))
        futures (map apply-move-to-board* moves)]
    (reduce #(or %1 %2) false (map check? futures))))

(def wpawn (Piece. WHITE PAWN false))
(def wrook (Piece. WHITE ROOK false))
(def wknight (Piece. WHITE KNIGHT false))
(def wbishop (Piece. WHITE BISHOP false))
(def wqueen (Piece. WHITE QUEEN false))
(def wking (Piece. WHITE KING false))
(def bpawn (Piece. BLACK PAWN false))
(def brook (Piece. BLACK ROOK false))
(def bknight (Piece. BLACK KNIGHT false))
(def bbishop (Piece. BLACK BISHOP false))
(def bqueen (Piece. BLACK QUEEN false))
(def bking (Piece. BLACK KING false))

(def default-game
  (GameState.
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
    (Coord. 0 7) wrook
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
    (Coord. 7 6) wpawn} {}))
