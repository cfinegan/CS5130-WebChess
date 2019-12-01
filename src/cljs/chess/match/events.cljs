(ns chess.match.events
  (:require
   [re-frame.core :as re-frame]
   [chess.chess :as chess]
   [chess.db :as db]
   [chess.match.subs :as subs]
   ))

(defn write-json-str [obj]
  (.stringify js/JSON (clj->js obj)))

(def match-path (re-frame/path :match))

(re-frame/reg-event-fx
 ::board-click
 [match-path]
 (fn [cofx [_ x y]]
   (let [db (:db cofx) 
         history (:history db)
         game (last history)
         board (:board game)
         selection (:selection db)
         team (:team db)
         active-team (if (= 0 (mod (count history) 2))
                       chess/BLACK
                       chess/WHITE)
         click-pos (chess/->Coord x y)]
     (cond
       ;; Do nothing if it's the opponent's turn.
       ;; TODO: Add an invalid selection animation.
       (or (not (= team active-team))
           (:game-over? db))
       {:db db}
       ;; If a piece is already selected...
       selection
       (let [{sel-pos :pos valid-moves :valid-moves} selection]
         (if (= sel-pos click-pos)
           ;; Selecting the same piece twice de-selects.
           {:db (assoc db :selection nil)}
           ;; Else try to move the piece.
           (let [move (chess/->Move sel-pos click-pos)]
             (if (some #(= click-pos %) valid-moves)
               ;; Update game state if the move is valid
               (let [new-game (chess/apply-move game move)]
                 (do
                   (.send db/conn
                          (write-json-str
                           {:type :move
                            :from sel-pos
                            :to click-pos}))
                   {:db (assoc db
                               :history (conj history new-game)
                               :selection nil)}))
               ;; Otherwise do nothing.
               ;; TODO: Add an invalid selection animation.
               {:db db}))))
       ;; Otherwise attempt to select.
       :else
       (let [piece (board click-pos)]
         (if (and piece (= (:team piece) team))
           ;; Only select when the space holds a friendly piece.
           (let [valid-moves (chess/valid-moves history click-pos true)
                 caps (:captures game)
                 enemy (chess/find-team board (chess/other-team team))
                 futures (map
                          #(vector
                            %
                            (chess/apply-move
                             game
                             (chess/->Move click-pos %)))
                          valid-moves)
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
                                     (fn [m] [% (chess/->Move e m)])
                                     (chess/valid-moves
                                      (conj history (peek %))
                                      e
                                      true)))
                                  enemy))
                               futures))
                 enemy-futures (map
                                #(vector
                                  (first (first %))
                                  (peek
                                   (chess/apply-move-to-board
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
                             valid-moves)
                 capture-moves (filter
                                (fn [m]
                                  (some
                                   #(and
                                     (= m (first %))
                                     (not (empty? (peek %))))
                                   future-captures))
                                valid-moves)]
             {:db (assoc db :selection {:pos click-pos
                                        :valid-moves valid-moves
                                        :vuln-moves vuln-moves
                                        :capture-moves capture-moves})})
           ;; Otherwise do nothing
           ;; TODO: Add an invalid selection animation.
           {:db db}))))))

(re-frame/reg-event-fx
 ::invalid-move
 [match-path]
 (fn [cofx _]
   (let [db (:db cofx)
         history (:history db)]
     {:db (assoc db
                 :history (pop history)
                 :server-forced-undo? true)})))

(re-frame/reg-event-fx
 ::opponent-moved
 [match-path]
 (fn [cofx [_ msg]]
   (let [db (:db cofx)
         history (:history db)
         game (last history)
         check? (:check? msg)
         game-over? (:game-over? msg)
         from-x (:x (:from msg))
         from-y (:y (:from msg))
         to-x (:x (:to msg))
         to-y (:y (:to msg))
         from (chess/->Coord from-x from-y)
         to (chess/->Coord to-x to-y)
         move (chess/->Move from to)
         new-game (chess/apply-move game move)]
     {:db (assoc db
                 :history (conj history new-game)
                 :check? check?
                 :game-over? game-over?)})))

(re-frame/reg-event-fx
 ::game-over
 [match-path]
 (fn [cofx _]
   (let [db (:db cofx)]
     {:db (assoc db :game-over? true)})))
