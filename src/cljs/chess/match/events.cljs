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
       (not (= team active-team))
       {:db db}
       ;; If a piece is already selected...
       selection
       (let [{sel-pos :pos valid-moves :valid-moves} selection]
         (if (= sel-pos click-pos)
           ;; Selecting the same piece twice de-selects.
           {:db (assoc db :selection nil)}
           ;; Else try to move the piece.
           (let [move (chess/->Move sel-pos click-pos)]
             (println "move: " move)
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
           (let [valid-moves (chess/valid-moves history click-pos true)]
             {:db (assoc db :selection {:pos click-pos
                                        :valid-moves valid-moves})})
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
         move (chess/->Move (:from msg) (:to msg))
         new-game (chess/apply-move game move)]
     (println "from: " (:from msg))
     (println "to: " (:to msg))
     {:db (assoc db :history (conj history new-game))})))
