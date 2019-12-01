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
(defrecord Piece [team type moved? id])

;; 'board' is a hash table mapping coords to pieces
;; Ex. (board (Coord 0 0)) => (Piece :rook :black false 1)
;; 'captures' is a hash table mapping [team type] to number of captures
;; Ex. (caps [WHITE PAWN]) => 2
;;     (caps [BLACK KING]) => nil
;; Use 0 as a default value to get an accurate count when none are captured
;;     (caps [BLACK KING] 0) => 0
(defrecord GameState [board captures])

(defrecord Move [from to])

(defrecord Rules [self-check? en-passant?])

(defrecord Opportunities [vuln-moves capture-moves])

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
(defn valid-moves [history {px :x py :y :as coord} check-castle?]
  (let [board (:board (last history))
        {team :team type :type moved? :moved? :as piece} (board coord)]
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
                       (list c) nil))
             regular-moves (concat fwd1 fwd2 ldiag rdiag)
             fifth-rank? (if (= team WHITE) (= py 3) (= py 4))] 
         (if fifth-rank?
           (let [adj-left-pos (Coord. (- px 1) py)
                 adj-right-pos (Coord. (+ px 1) py)
                 adj-left (board adj-left-pos)
                 adj-right (board adj-right-pos)
                 old-board (:board (last (pop history)))
                 pawn-left? (and adj-left
                                 (= (:type adj-left) PAWN)
                                 (= (:team adj-left) (other-team team))
                                 (let [orig-pos (if (= (:team adj-left) WHITE)
                                                  (Coord. (- px 1) 6)
                                                  (Coord. (- px 1) 1))]
                                   (and (not (board orig-pos))
                                        (old-board orig-pos)
                                        (= (:type (old-board orig-pos)) PAWN)
                                        (not (:moved? (old-board orig-pos))))))
                 pawn-right? (and adj-right
                                  (= (:type adj-right) PAWN)
                                  (= (:team adj-right) (other-team team))
                                  (let [orig-pos (if (= (:team adj-right) WHITE)
                                                   (Coord. (+ px 1) 6)
                                                   (Coord. (+ px 1) 1))]
                                    (and (not (board orig-pos))
                                         (old-board orig-pos)
                                         (= (:type (old-board orig-pos)) PAWN)
                                         (not (:moved? (old-board orig-pos))))))
                 new-y (if (= team WHITE) (- py 1) (+ py 1))
                 en-passant-left (if pawn-left?
                                   (list (Coord. (- px 1) new-y)) nil)
                 en-passant-right (if pawn-right?
                                    (list (Coord. (+ px 1) new-y)) nil)]
             (concat regular-moves en-passant-left en-passant-right))
           regular-moves))
       
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
       (let [regular-moves (for [{x :x y :y} king-moves
                                 :let [coord* (Coord. (+ x px) (+ y py))
                                       piece* (board coord*)]
                                 :when (not (and piece*
                                                 (= team (:team piece*))))]
                             coord*)]
         (if check-castle?
           (let [enemy (find-team board (other-team team))
                 enemy-moves (flatten
                              (map #(valid-moves history % false) enemy))
                 in-check? (loop [m enemy-moves]
                             (cond (empty? m) false
                                   (= coord (first m)) true
                                   :else (recur (rest m))))]
             (if (and (not in-check?) (not moved?))
               (let [unmoved-rooks (filter #(and (= (:type (board %)) ROOK)
                                                 (not (:moved? (board %))))
                                           (find-team board team))]
                 (if (not (empty? unmoved-rooks))
                   (let [l-rook (first (filter #(= (:x %) 0) unmoved-rooks))
                         r-rook (first (filter #(= (:x %) 7) unmoved-rooks))
                         l-accum (accum-tiles sub1 stay board team coord)
                         r-accum (accum-tiles add1 stay board team coord)
                         l-castle (if (and l-rook
                                           (not (empty? l-accum))
                                           (some #(= (Coord. (+ (:x l-rook) 1)
                                                             (:y l-rook))
                                                     %)
                                                 l-accum)
                                           (not (some (fn [p1]
                                                        (some (fn [p2]
                                                                (= p1 p2))
                                                              l-accum))
                                                      enemy-moves)))
                                    (list (Coord. (+ (:x l-rook) 2)
                                                  (:y l-rook)))
                                    nil)
                         r-castle (if (and r-rook
                                           (not (empty? r-accum))
                                           (some #(= (Coord. (- (:x r-rook) 1)
                                                             (:y r-rook))
                                                     %)
                                                 r-accum)
                                           (not (some (fn [p1]
                                                        (some (fn [p2]
                                                                (= p1 p2))
                                                              r-accum))
                                                      enemy-moves)))
                                    (list (Coord. (- (:x r-rook) 1)
                                                  (:y r-rook)))
                                    nil)]
                     (concat regular-moves l-castle r-castle))
                   regular-moves))
               regular-moves))
           regular-moves))

       (= type QUEEN)
       (concat (accum-straight board team coord)
               (accum-diagonal board team coord))))))

(defn apply-move-to-board [board {from :from to :to}]
  (let [{team :team type :type moved? :moved? id :id :as piece} (board from)
        move-piece (if moved? piece (Piece. team type true id))
        castle? (and (= type KING)
                     (= (:y from) (:y to))
                     (> (abs (- (:x to) (:x from))) 1))
        en-passant? (and (= type PAWN)
                         (= (abs (- (:x to) (:x from))) 1)
                         (= (abs (- (:y to) (:y from))) 1)
                         (not (board to)))
        promotion? (and (= type PAWN)
                        (if (= team WHITE)
                          (= (:y to) 0)
                          (= (:y to) 7)))]
    (cond
      castle?
      (let [old-rook-pos (if (< (:x to) 4)
                           (Coord. 0 (:y to))
                           (Coord. 7 (:y to)))
            old-rook-piece (board old-rook-pos)
            new-rook-piece (Piece. team ROOK true (:id old-rook-piece))
            new-rook-pos (if (< (:x to) 4) ;; queen's side
                           (Coord. 3 (:y to))
                           (Coord. 5 (:y to)))
            new-king-pos (if (< (:x to) 4)
                           (Coord. 2 (:y to))
                           (Coord. 6 (:y to)))]
        [(-> board
             (dissoc from)
             (dissoc old-rook-pos)
             (assoc new-rook-pos new-rook-piece)
             (assoc new-king-pos move-piece))
         nil])

      en-passant?
      (let [captured-pawn-pos (Coord. (:x to) (:y from))]
        [(-> board
             (dissoc from)
             (dissoc captured-pawn-pos)
             (assoc to move-piece))
         (board captured-pawn-pos)])

      promotion?
      (let [new-piece (Piece. team QUEEN true id)]
        [(-> board
             (dissoc from)
             (assoc to new-piece))
         (board to)])
        
      :else
      [(-> board
           (dissoc from)
           (assoc to move-piece))
       (board to)])))
      
(defn apply-move-to-board* [board move]
  (first (apply-move-to-board board move)))

(defn apply-move [{board :board caps :captures}
                  {from :from to :to :as move}]
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

(defn active-team [history]
  (if (= 0 (mod (count history) 2)) BLACK WHITE))

(defn team->string [team]
  (cond (= team WHITE) "white"
        (= team BLACK) "black"))

(defn check? [team history]
  (let [game (last history)
        board (:board game)
        king (find-king board team)
        enemy (find-team board (other-team team))
        moves (flatten
               (map #(valid-moves history % true) enemy))]
    (loop [m moves]
      (cond (empty? m) false
            (= king (first m)) true
            :else (recur (rest m))))))

(defn check-mate? [team history]
  (let [game (last history)
        board (:board game)
        friends (find-team board team)
        moves (flatten
               (map #(map (fn [to] (Move. % to))
                          (valid-moves history % true))
                    friends))
        futures (map #(apply-move game %) moves)]
    (reduce #(and %1 %2)
            (map #(check? team (conj history %))
                 futures))))

(defn opportunities [history team pos moves]
  (let [game (last history)
        board (:board game)
        caps (:captures game)
        piece (board pos)
        enemy (find-team board (other-team team))
        futures (map
                 #(vector
                   %
                   (apply-move
                    game
                    (Move. pos %)))
                 moves)
        future-captures (map
                         (fn [f]
                           [(first f)
                            (filter
                             (fn [c]
                               (not (some #(= c %) caps)))
                             (:captures (peek f)))])
                         futures)
        enemy-moves (apply
                     concat
                     (map
                      #(apply
                        concat
                        (map
                         (fn [e]
                           (map
                            (fn [m] [% (Move. e m)])
                            (valid-moves
                             (conj history (peek %))
                             e
                             true)))
                         enemy))
                      futures))
        enemy-futures (map
                       #(vector
                         (first (first %))
                         (peek
                          (apply-move-to-board
                           (:board (peek (first %)))
                           (peek %))))
                       enemy-moves)
        enemy-captures (filter
                        #(and (peek %)
                              (not (empty? (peek %)))
                              (= (:id (peek %))
                                 (:id piece)))
                        enemy-futures)
        vuln-moves (filter
                    (fn [m] (some #(= m (first %)) enemy-captures))
                    moves)
        capture-moves (filter
                       (fn [m]
                         (some
                          #(and
                            (= m (first %))
                            (not (empty? (peek %))))
                          future-captures))
                       moves)]
    (Opportunities. vuln-moves capture-moves)))

(defn wpawn [id] (Piece. WHITE PAWN false id))
(defn wrook [id] (Piece. WHITE ROOK false id))
(defn wknight [id] (Piece. WHITE KNIGHT false id))
(defn wbishop [id] (Piece. WHITE BISHOP false id))
(defn wqueen [id] (Piece. WHITE QUEEN false id))
(defn wking [id] (Piece. WHITE KING false id))
(defn bpawn [id] (Piece. BLACK PAWN false id))
(defn brook [id] (Piece. BLACK ROOK false id))
(defn bknight [id] (Piece. BLACK KNIGHT false id))
(defn bbishop [id] (Piece. BLACK BISHOP false id))
(defn bqueen [id] (Piece. BLACK QUEEN false id))
(defn bking [id] (Piece. BLACK KING false id))

(def default-game
  (GameState.
   {(Coord. 0 0) (brook 1)
    (Coord. 7 0) (brook 2)
    (Coord. 1 0) (bknight 3)
    (Coord. 6 0) (bknight 4)
    (Coord. 2 0) (bbishop 5)
    (Coord. 5 0) (bbishop 6)
    (Coord. 3 0) (bqueen 7)
    (Coord. 4 0) (bking 8)
    (Coord. 0 1) (bpawn 9)
    (Coord. 1 1) (bpawn 10)
    (Coord. 2 1) (bpawn 11)
    (Coord. 3 1) (bpawn 12)
    (Coord. 4 1) (bpawn 13)
    (Coord. 5 1) (bpawn 14)
    (Coord. 6 1) (bpawn 15)
    (Coord. 7 1) (bpawn 16)
    (Coord. 0 7) (wrook 17)
    (Coord. 7 7) (wrook 18)
    (Coord. 1 7) (wknight 19)
    (Coord. 6 7) (wknight 20)
    (Coord. 2 7) (wbishop 21)
    (Coord. 5 7) (wbishop 22)
    (Coord. 3 7) (wqueen 23)
    (Coord. 4 7) (wking 24)
    (Coord. 0 6) (wpawn 25)
    (Coord. 1 6) (wpawn 26)
    (Coord. 2 6) (wpawn 27)
    (Coord. 3 6) (wpawn 28)
    (Coord. 4 6) (wpawn 29)
    (Coord. 5 6) (wpawn 30)
    (Coord. 6 6) (wpawn 31)
    (Coord. 7 6) (wpawn 32)}
   {}))
