(ns gamer_namespace
  (:import [org.ggp.base.player.gamer.statemachine StateMachineGamer]
           [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]))

(def print-depth 3)
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
          (get-in game-state [:actions role]))]

    (-> game-state
        (assoc :state
          (.getNextState (:game game-state)
                         (:state game-state)
                         ordered-actions))
        (dissoc :actions))))

(defn move [game-state role action]
  (let [state
        (-> game-state
            (update-in [:depth] inc)
            (assoc-in [:actions role] action))]
    (if (= 2 (count (:actions game-state)))
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

(defn min-score [game-state]
  (log game-state "MIN" (:action game-state))

  (logging-depth
   [game-state "MIN VAL"]
   (unless-terminal game-state
    (log game-state "MIN???" (.isTerminal (:game game-state) (:state game-state)))
    (let [scores
          (into [] (map #(max-score (move game-state (:opponent-role game-state) %))
                        (opponent-legal-moves game-state)))]
      (log game-state "min considering" scores)
      (reduce min 100 scores)))))

(defn max-score [game-state]
  (bump! game-state)
  (log game-state "MAX" (:opponent-action game-state))

  (logging-depth
   [game-state "MAX VAL"]
   (unless-terminal game-state
    (let [scores
          (into [] (map #(min-score (move game-state (:role game-state) %))
                        (my-legal-moves game-state)))]
      (log game-state "max considering" scores)
      (reduce max 0 scores)))))


(defn best-move [game-state]
  (let [actions (my-legal-moves game-state)]
    (if (= 1 (count actions))
      (first actions)
      (let [better-state
            #(max-key second %1 %2)

            gen-tree
            #(min-score (-> game-state
                            (assoc :depth 0)
                            (move (:role game-state) %)))]
        (reduce better-state (map (juxt identity gen-tree) actions))))))

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


