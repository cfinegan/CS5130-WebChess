(ns chess.events
  (:require
   [re-frame.core :as re-frame]
   [chess.subs :as subs]
   [chess.db :as db]
   [chess.chess :as chess]
   ))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-fx
 :board-click
 (fn [cofx [_ x y]]
   (let [history (re-frame/subscribe [::subs/history])
         selection (re-frame/subscribe [::subs/selection])
         gameover (re-frame/subscribe [::subs/gameover])
         game (last @history)
         board (:board game)
         caps (:captures game)
         team (if (= 0 (mod (count @history) 2)) chess/BLACK chess/WHITE)
         pos (chess/->Coord x y)
         piece (board pos)]
     ;; TODO: Maybe break this cond out into different functions for when
     ;; a piece is selected (and thus the intent is to move it) and when
     ;; a piece is not selected (and thus the intent is to select it).
     ;; This may improve readability.
     (cond
       @gameover
       {:db (:db cofx)}
       ;; If a piece is selected, this click is to move it
       @selection
       (let [moves (chess/valid-moves board @selection)]
         (if (some #(= pos %) moves)
           (let [new-game (chess/apply-move game (chess/->Move @selection pos))
                 otherteam (chess/other-team team)
                 winner (chess/winner (:captures new-game))
                 checkmate (chess/check-mate? (:board new-game) otherteam)]
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
                                         (cond (chess/check? (:board new-game) otherteam)
                                               (str " and " (chess/team->string otherteam) " is in check")
                                               (chess/check? (:board new-game) team)
                                               (str " and " (chess/team->string team) " is in check")
                                               :else "")))}))
           (if (= @selection pos)
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
       (not @selection)
       {:db (assoc (:db cofx)
                   :selection pos
                   :message (str "select a destination"
                                 (if (chess/check? board team)
                                   " (you are in check!)"
                                   "")))}
       ;; Base case shows error for debugging
       :else
       {:db (assoc (:db cofx) :message "board-click is in an unknown state!")}))))
