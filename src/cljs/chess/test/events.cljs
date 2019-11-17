(ns chess.test.events
  (:require
   [re-frame.core :as re-frame]
   [chess.test.subs :as subs]
   [chess.test.db :as db]
   [chess.chess :as chess]
   ))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-fx
 :undo-click
 (fn [cofx _]
   (let [history (re-frame/subscribe [::subs/history])
         len (count @history)]
     (if (> len 1)
       {:db (assoc (:db cofx)
                   :history (pop @history)
                   :gameover false
                   :message "reverted to previous game state"
                   :selection nil)}
       (throw (js/Error. "Cannot undo board in starting position."))))))

(re-frame/reg-event-fx
 :board-click
 (fn [cofx [_ x y]]
   (let [history (re-frame/subscribe [::subs/history])
         view-selection (re-frame/subscribe [::subs/selection])
         active-team (re-frame/subscribe [::subs/active-team])
         rotate? (and active-team (= @active-team chess/BLACK))
         selection (and @view-selection
                        (if rotate?
                          (chess/rotate-coord @view-selection)
                          @view-selection))
         gameover (re-frame/subscribe [::subs/gameover])
         team @(re-frame/subscribe [::subs/active-team])
         game (last @history)
         board (:board game)
         view-board (if rotate? (chess/rotate-board board) board)
         caps (:captures game)
         pos (chess/->Coord x y)
         view-pos (if rotate? (chess/rotate-coord pos) pos)
         piece (view-board pos)]
     ;; TODO: Maybe break this cond out into different functions for when
     ;; a piece is selected (and thus the intent is to move it) and when
     ;; a piece is not selected (and thus the intent is to select it).
     ;; This may improve readability.
     (cond
       @gameover
       {:db (:db cofx)}
       ;; If a piece is selected, this click is to move it
       selection
       (let [moves (chess/valid-moves board selection @history true)]
         (if (some #(= view-pos %) moves)
           (let [move (chess/->Move selection view-pos)
                 new-game (assoc (chess/apply-move game move)
                                 :last-move move)
                 otherteam (chess/other-team team)
                 winner (chess/winner (:captures new-game))
                 checkmate (chess/check-mate?
                            new-game
                            otherteam
                            (conj @history new-game))]
             (if (or (= winner team) checkmate)
               {:db (assoc (:db cofx)
                           :history (conj @history new-game)
                           :selection nil
                           :gameover true
                           :message (str (chess/team->string team)
                                         " is the winner"
                                         (if checkmate " (checkmate)" "")))}
               {:db (assoc (:db cofx)
                           :history (conj @history new-game)
                           :selection nil
                           :message (str "the move is valid"
                                         (cond (chess/check?
                                                new-game
                                                otherteam
                                                (conj @history new-game))
                                               (str " and "
                                                    (chess/team->string
                                                     otherteam)
                                                    " is in check")
                                               (chess/check?
                                                new-game
                                                team
                                                (conj @history new-game))
                                               (str " and "
                                                    (chess/team->string team)
                                                    " is in check")
                                               :else "")))}))
           (if (= selection view-pos)
             {:db (assoc (:db cofx)
                         :history @history
                         :selection nil)}
             {:db (assoc (:db cofx) :message "the move isn't valid")})))
       ;; Else if click was an empty space, or an enemy piece, show error
       (not piece)
       {:db (assoc (:db cofx) :message "cannot select empty space")}
       (not (= (:team piece) team))
       {:db (assoc (:db cofx) :message "cannot select opposite team")}
       ;; Else if no piece is selected, make this click new selection
       (not selection)
       {:db (assoc (:db cofx)
                   :selection pos
                   :message (str "select a destination"
                                 (if (chess/check? game team @history)
                                   " (you are in check!)" "")))}
       ;; Base case shows error for debugging
       :else
       {:db (assoc (:db cofx) :message "board-click is in an unknown state!")}))))
