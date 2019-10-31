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
;; 'captures' is a hash table mapping [team type] to number of captures
;; Ex. (caps [WHITE PAWN]) => 2
;;     (caps [BLACK KING]) => nil
;; Use 0 as a default value to get an accurate count when none are captured
;;     (caps [BLACK KING] 0) => 0
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
          (conj out pos)
          ;; Otherwise keep searching
          :else
          (let [new-x (x-xform (:x pos))
                new-y (y-xform (:y pos))]
            (recur (Coord. new-x new-y) (conj out pos))))))))

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

(declare find-team)
(declare other-team)

;; TODO: Right now, this assumes that a piece actually exists
;; at 'coord'. We should probably return some bottom value
;; when the piece doesn't exist, distinct from '() since '()
;; is also returned when the piece exists but has no valid moves.
(defn valid-moves [board {px :x py :y :as coord}]
  (let [{team :team type :type moved? :moved? :as piece} (board coord)]
    (filter
     valid-coord?
     (cond
       (= type PAWN)
       (let [dir (if (= team WHITE) -1 1)
             fwd1 (let [c (Coord. px (+ dir py))]
                    (if (board c) nil (list c)))
             fwd2 (if (and (not moved?) fwd1)
                    (let [c (Coord. px (+ dir dir py))]
                      (if (board c) nil (list c)))
                    nil)
             ldiag (let [c (Coord. (- px 1) (+ dir py))
                         p (board c)]
                     (if (and p (not (= (:team p) team)))
                       (list c) nil))
             rdiag (let [c (Coord. (+ px 1) (+ dir py))
                         p (board c)]
                     (if (and p (not (= (:team p) team)))
                       (list c) nil))]
         (concat fwd1 fwd2 ldiag rdiag))
       
       (= type ROOK)
       (accum-straight board team coord)
       
       (= type BISHOP)
       (accum-diagonal board team coord)
       
       (= type KNIGHT)
       (for [{x :x y :y} knight-moves
             :let [coord* (Coord. (+ x px) (+ y py))
                   piece* (board coord*)]
             :when (not (and piece* (= team (:team piece*))))]
         coord*)
       
       (= type KING)
       (let [enemy (filter #(not (= (:type (board %)) KING)) (find-team board (other-team team)))
             enemy-king* (filter #(= (:type (board %)) KING) (find-team board (other-team team)))
             enemy-moves (concat
                          (flatten (map #(valid-moves board %) enemy))
                          (if (not (empty? enemy-king*))
                            (let [enemy-king (first enemy-king*)]
                              (for [{x :x y :y} king-moves
                                    :let [coord* (Coord. (+ x (:x enemy-king)) (+ y (:y enemy-king)))
                                          piece* (board coord*)]
                                    :when (not (and piece* (= (other-team team) (:team piece*))))]
                                coord*))
                            nil))
             in-check? (loop [m enemy-moves]
                         (cond (empty? m) false
                               (= coord (first m)) true
                               :else (recur (rest m))))
             regular-moves (for [{x :x y :y} king-moves
                                 :let [coord* (Coord. (+ x px) (+ y py))
                                       piece* (board coord*)]
                                 :when (not (and piece* (= team (:team piece*))))]
                             coord*)]
         (if (and (not in-check?)
                  (not moved?))
           (let [unmoved-rooks (filter #(and (= (:type (board %)) ROOK) (not (:moved? (board %)))) (find-team board team))]
             (if (not (empty? unmoved-rooks))
               (let [l-rook (filter #(= (:x %) 0) unmoved-rooks)
                     r-rook (filter #(= (:x %) 7) unmoved-rooks)
                     l-accum (accum-tiles sub1 stay board team coord)
                     r-accum (accum-tiles add1 stay board team coord)
                     l-castle (if (and (not (empty? l-rook))
                                       (not (empty? l-accum))
                                       (some #(= (Coord. (+ (:x (first l-rook)) 1) (:y (first l-rook))) %)
                                             l-accum)
                                       (not (some (fn [p1] (some (fn [p2] (= p1 p2)) l-accum)) enemy-moves)))
                                l-rook nil)
                     r-castle (if (and (not (empty? r-rook))
                                       (not (empty? r-accum))
                                       (some #(= (Coord. (- (:x (first r-rook)) 1) (:y (first r-rook))) %)
                                             r-accum)
                                       (not (some (fn [p1] (some (fn [p2] (= p1 p2)) r-accum)) enemy-moves)))
                                r-rook nil)]
                 (concat regular-moves l-castle r-castle))
               regular-moves))
           regular-moves))

       (= type QUEEN)
       (concat (accum-straight board team coord)
               (accum-diagonal board team coord))))))

(defn apply-move-to-board [board {from :from to :to}]
  (let [{team :team type :type moved? :moved? :as piece} (board from)
        move-piece (if moved? piece (Piece. team type true))
        castle? (and (board to)
                     (= (:type (board from)) KING)
                     (= (:type (board to)) ROOK)
                     (= (:team (board from)) (:team (board to)))
                     (not (:moved? (board from))) ;; we already check this in valid-moves
                     (not (:moved? (board to))))] ;; but it doesn't hurt to verify again
    (if castle?
      (let [new-rook-piece (Piece. team ROOK true)
            new-rook-pos (if (< (:x to) 4) ;; queen's side
                           (Coord. 3 (:y to))
                           (Coord. 5 (:y to)))
            new-king-pos (if (< (:x to) 4)
                           (Coord. 2 (:y to))
                           (Coord. 6 (:y to)))]
        [(-> board
             (dissoc from)
             (dissoc to)
             (assoc new-rook-pos new-rook-piece)
             (assoc new-king-pos move-piece))
         nil])
      [(-> board
           (dissoc from)
           (assoc to move-piece))
       (board to)])))
      
(defn apply-move-to-board* [board move]
  (first (apply-move-to-board board move)))

(defn apply-move [{board :board caps :captures} {from :from to :to :as move}]
  (let [[new-board cap-piece] (apply-move-to-board board move)]
    (GameState.
     new-board
     (if cap-piece
       (let [{team :team type :type} cap-piece
             team+type [team type]]
         (assoc caps team+type (inc (caps team+type 0))))
       caps))))

(defn winner [caps]
  (cond (caps [BLACK KING]) WHITE
        (caps [WHITE KING]) BLACK))

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

(defn team->string [team]
  (cond (= team WHITE) "white"
        (= team BLACK) "black"))

(defn check? [board team]
  (let [king (find-king board team)
        enemy (find-team board (other-team team))
        moves (flatten (map #(valid-moves board %1) enemy))]
    (loop [m moves]
      (cond (empty? m) false
            (= king (first m)) true
            :else (recur (rest m))))))

(defn check-mate? [board team]
  (let [friends (find-team board team)
        moves (flatten (map #(map (fn [to] (Move. %1 to)) (valid-moves board %1)) friends))
        futures (map #(apply-move-to-board* board %1) moves)]
    ;; if all future moves put us in check, then we've been checkmated
    (reduce #(and %1 %2) (map #(check? %1 team) futures))))

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
