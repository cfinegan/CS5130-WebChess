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
         rules (:rules db)
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
           (:undo? db)
           (:opponent-undo? db))
       {:db (assoc db :bad-select? click-pos)}
       ;; If a piece is already selected...
       selection
       (let [{sel-pos :pos valid-moves :valid-moves} selection]
         (if (= sel-pos click-pos)
           ;; Selecting the same piece twice de-selects.
           {:db (assoc db :selection nil :bad-select? false)}
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
                               :selection nil
                               :bad-select? nil
                               :server-forced-undo? false
                               :server-forced-undo-msg nil)}))
               ;; Otherwise do nothing.
               ;; TODO: Add an invalid selection animation.
               {:db (assoc db :bad-select? click-pos)}))))
       ;; Otherwise attempt to select.
       :else
       (let [piece (board click-pos)]
         (if (and piece (= (:team piece) team))
           ;; Only select when the space holds a friendly piece.
           (let [valid-moves (chess/valid-moves rules
                                                history
                                                click-pos
                                                true)
                 valid-moves* (if (:self-check? rules)
                                valid-moves
                                (filter #(not
                                          (chess/check?
                                           rules
                                           team
                                           (conj
                                            history
                                            (chess/apply-move
                                             game
                                             (chess/->Move click-pos %)))))
                                        valid-moves))
                 opps (when (:color-tiles? rules)
                        (chess/opportunities rules
                                             history
                                             team
                                             click-pos
                                             valid-moves*))]
             {:db (assoc db
                         :selection {:pos click-pos
                                     :valid-moves valid-moves*
                                     :vuln-moves (:vuln-moves opps)
                                     :capture-moves (:capture-moves opps)}
                         :bad-select? nil
                         :server-forced-undo? false
                         :server-forced-undo-msg nil)})
           ;; Otherwise do nothing
           ;; TODO: Add an invalid selection animation.
           {:db (assoc db :bad-select? click-pos)}))))))

(re-frame/reg-event-fx
 ::invalid-move
 [match-path]
 (fn [cofx [_ msg]]
   (let [db (:db cofx)
         history (:history db)]
     {:db (assoc db
                 :history (pop history)
                 :server-forced-undo? true
                 :server-forced-undo-msg (:msg msg))})))

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
 (fn [cofx [_ msg]]
   (let [db (:db cofx)]
     {:db (assoc db
                 :game-over? true
                 :undo? false
                 :opponent-undo? false
                 :server-forced-undo? false
                 :server-forced-undo-msg nil)})))

(re-frame/reg-event-fx
 ::leave-game-click
 [match-path]
 (fn [cofx _]
   (let [db (:db cofx)]
     (do
       (.send db/conn (write-json-str {:type :leave}))
       {:db (assoc db :leaving? true)}))))

(re-frame/reg-event-fx
 ::forfeit-game-click
 [match-path]
 (fn [cofx _]
   (let [db (:db cofx)]
     (do
       (.send db/conn (write-json-str {:type :forfeit}))
       {:db (assoc db :forfeit? true)}))))

(re-frame/reg-event-fx
 ::bad-leave
 [match-path]
 (fn [cofx _]
   (let [db (:db cofx)]
     {:db (assoc db :leaving? false)})))

(re-frame/reg-event-fx
 ::bad-forfeit
 [match-path]
 (fn [cofx _]
   (let [db (:db cofx)]
     {:db (assoc db :forfeit? false)})))

(re-frame/reg-event-fx
 ::bad-undo
 [match-path]
 (fn [cofx _]
   (let [db (:db cofx)]
     {:db (assoc db :undo? false)})))

(re-frame/reg-event-fx
 ::request-undo-click
 [match-path]
 (fn [cofx _]
   (let [db (:db cofx)]
     (do
       (.send db/conn (write-json-str {:type :undo-request}))
       {:db (assoc db :undo? true)}))))

(re-frame/reg-event-fx
 ::respond-undo-click
 [match-path]
 (fn [cofx [_ accept?]]
   (let [db (:db cofx)]
     (do
       (.send db/conn
              (write-json-str {:type :undo-response
                               :accept? accept?}))
       {:db (assoc db :undo? true)}))))

(re-frame/reg-event-fx
 ::undo-request
 [match-path]
 (fn [cofx _]
   (let [db (:db cofx)]
     {:db (assoc db :opponent-undo? true)})))

(re-frame/reg-event-fx
 ::undo-response
 [match-path]
 (fn [cofx [_ msg]]
   (let [db (:db cofx)
         accept? (:accept? msg)
         history (:history db)]
     {:db (assoc db
                 :history (if accept?
                            (pop history)
                            history)
                 :undo? false
                 :opponent-undo? false)})))
