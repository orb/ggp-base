(ns gamer_namespace
  (:import [org.ggp.base.player.gamer.statemachine StateMachineGamer]
           [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]))

(def print-depth 5)
(defn spaces [n] (apply str (take n (repeat " "))))
(defn log [game-state & args]
  (let [depth (:depth game-state)]
    (when (< depth print-depth)
      (apply println (spaces depth) depth args))))

(defmacro logging-depth [[game-state message] & body]
  `(let [val# (do ~@body)]
     (log ~game-state ~message val#)
     val#))

(defn find-opponent [game role]
  (first (filter #(not (= role %)) (.getRoles game))))

(defn my-legal-moves [game-state]
  (.getLegalMoves (:game game-state)
                  (:state game-state)
                  (:role game-state)))

(defn opponent-legal-moves [game-state]
  (.getLegalMoves (:game game-state)
                  (:state game-state)
                  (:opponent-role game-state)))

;; applies moves for all roles
(defn next-state [game-state]
  (let [ordered-actions
        (for [role (.getRoles (:game game-state))]
          (get-in game-state [:actions role]))
        next-state
        (.getNextState (:game game-state)
                       (:state game-state)
                       ordered-actions)]

    (-> game-state
        (assoc :state next-state)
        (assoc :last-actions ordered-actions)
        (dissoc :actions))))

(defn move [game-state role action]
  (let [state
        (-> game-state
            (update-in [:depth] (fnil inc 0))
            (assoc-in [:actions role] action)
            (assoc-in [:last-actions] [action]))]
    (if (= 2 (count (:actions state)))
      (next-state state)
      state)))

(defn bump! [game-state]
  (if (zero? (mod (swap! (:count game-state) inc) 10000))
    (println "COUNT" @(:count game-state))))

;; ----------------------------------------
(def max-score)
(def min-score)

(defmacro unless-terminal [game-state & body]
  `(if (.isTerminal (:game ~game-state) (:state ~game-state))
     (let [my-score#
           (.getGoal (:game ~game-state)
                     (:state ~game-state)
                     (:role ~game-state))
           opponent-score#
           (.getGoal (:game ~game-state)
                     (:state ~game-state)
                     (:opponent-role ~game-state))]
       (log ~game-state "GOAL " (:role ~game-state) "=" my-score#
            " " (:opponent-role ~game-state) "=" opponent-score#)
       my-score#)
     (do
       ~@body)))

;; (defn max-shortcut [current-max values]
;;   (if (or (>= current-max 100)
;;           (not (seq values)))
;;     current-max
;;     (recur (max current-max (first values))
;;            (rest values))))

;; (defn min-shortcut [current-min values]
;;   (if (or (<= current-min 0)
;;           (not (seq values)))
;;     current-min
;;     (recur (min current-min (first values))
;;            (rest values))))

(defn min-score [alpha beta game-state]
  (bump! game-state)
  (log game-state "MIN" alpha beta (:last-actions game-state))

  (logging-depth
   [game-state "MIN VAL"]
   (unless-terminal game-state
     (let [make-move #(move game-state (:opponent-role game-state) %)]
       (loop [beta beta
              next-moves (opponent-legal-moves game-state)]
         (cond
          (<= beta alpha) beta
          (empty? next-moves) beta
          :else (recur (min beta (max-score alpha beta (make-move (first next-moves))))
                       (rest next-moves))))))))


(defn max-score [alpha beta game-state]
  (bump! game-state)
  (log game-state "MAX" alpha beta (:last-actions game-state))

  (logging-depth [game-state "MAX VAL"]
                 (unless-terminal game-state
    (let [make-move #(move game-state (:role game-state) %)]
      (loop [alpha alpha
             next-moves (my-legal-moves game-state)]
        (cond
         (<= beta alpha) alpha
         (empty? next-moves) alpha
         :else (recur (max alpha (min-score alpha beta (make-move (first next-moves))))
                      (rest next-moves))))))))


(defn best-move [game-state]
  (let [actions (my-legal-moves game-state)]
    (if (= 1 (count actions))
      (first actions)
      (let [make-move #(move game-state (:role game-state) %)]
        (loop [alpha 0
               beta 100
               my-move nil
               next-moves (my-legal-moves game-state)]
          (cond
           (<= beta alpha) my-move
           (empty? next-moves) my-move
           :else (let [leaf-score (min-score alpha beta (make-move (first next-moves)))]
                   (if (> leaf-score alpha)
                     (recur leaf-score beta (first next-moves) (rest next-moves))
                     (recur alpha beta my-move (rest next-moves))))))))))

(defn test-solve [game]
  (let [counter (atom 0)
        initial-state {:game game
                       :state (.getInitialState game)
                       :count counter
                       :role (first (.getRoles game))
                       :opponent-role (second (.getRoles game))}
        move (best-move initial-state)]
    (println "SELECTING" (:role initial-state) move "after" @counter "nodes")
    move))

(defn week5-alphabeta []
  (proxy [StateMachineGamer] []
    (getInitialStateMachine []
      (ProverStateMachine.))

    (stateMachineSelectMove [timeout]
      (println "****************************************")
      (println "Making a move for" (.getRole this))
      (println "STATE:" (.getCurrentState this))
      (let [counter (atom 0)
            initial-state {:game (.getStateMachine this)
                           :state (.getCurrentState this)
                           :count counter
                           :role (.getRole this)
                           :opponent-role (find-opponent (.getStateMachine this) (.getRole this))}
            move (best-move initial-state)]
        (println "SELECTING" (:role initial-state) move "after" @counter "nodes")
        move))

    (stateMachineMetaGame [timeout]
      (println "SampleClojureGamer metagame called"))

    (stateMachineAbort []
      (println "SampleClojureGamer abort called"))

    (stateMachineStop []
      (println "SampleClojureGamer stop called"))))


