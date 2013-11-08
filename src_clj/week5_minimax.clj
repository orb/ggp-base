(ns gamer_namespace
  (:import [org.ggp.base.player.gamer.statemachine StateMachineGamer]
           [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]))



(def print-depth 2)
(defn spaces [n] (apply str (take n (repeat " "))))
(defn log [depth & args]
  (when (< depth print-depth)
    (apply println (spaces depth) depth args)))

(defmacro logging-depth [[depth message] & body]
  `(let [val# (do ~@body)]
     (log ~depth ~message val#)
     val#))

(defn find-opponent [game role]
  (first (filter #(not (= role %)) (.getRoles game))))

(defn next-state [game state action-map]
  (let [actions (for [role (.getRoles game)] (action-map role))]
    (.getNextState game state actions)))

(def counts (atom 0))
(defn bump! []
  (if (zero? (mod (swap! counts inc) 10000))
    (println "COUNT" @counts)))

(def max-score)
(defn min-score [depth game state role other-action]
  (bump!)
  (log depth "MIN" other-action)
  (let [opponent-role (find-opponent game role)]
    (logging-depth
     [depth "MIN VAL"]
     (if (.isTerminal game state)
       (do
         (log depth "GOAL " role "=" (.getGoal game state role)
              " " opponent-role "=" (.getGoal game state opponent-role))
         (.getGoal game state role))
       (let [opponent-actions
             (.getLegalMoves game state opponent-role)

             state-for-opponent-actions
             #(next-state game state {role other-action opponent-role %})

             next-states
             (map state-for-opponent-actions opponent-actions)

             scores
             (into [] (map #(max-score (inc depth) game %1 %2 role)
                           next-states
                           opponent-actions))]
         (log depth "min considering" scores)
         (reduce min 100 scores))))))

(defn max-score [depth game state action-taken role]
  (bump!)
  (log depth "MAX" action-taken)
  (let [opponent-role (find-opponent game role)]
    (logging-depth
     [depth "MAX VAL"]
     (if (.isTerminal game state)
       (do
         (log depth "GOAL " role "=" (.getGoal game state role)
              " " opponent-role "=" (.getGoal game state opponent-role))
         (.getGoal game state role))
       (let [scores (into [] (map #(min-score (inc depth) game state role %) (.getLegalMoves game state role)))]
         (log depth "max considering" scores)
         (reduce max 0 scores))))))

(defn best-move [game state role]
  (let [actions (.getLegalMoves game state role)]
    (if (= 1 (count actions))
      (first actions)
      (loop [actions actions
             best-action (first actions)
             best-score 0]
        (println "----------")
        (if (empty? actions)
          best-action
          (let [action (first actions)
                result (min-score 0 game state role action)]
            (println "BEST" role action "->" result)
            (cond
             (= result 100) action
             (> result best-score) (recur (rest actions) action result)
             :else (recur (rest actions) best-action best-score))))))))

(defn test-solve [game]
      (let [state-machine game
            current-state (.getInitialState game)
            role          (first (.getRoles game))
            _ (reset! counts 0)
            move (best-move state-machine current-state role)]
        (println "SELECTING" role move "after" @counts "nodes")
        move))

(defn week5-minimax []
  (proxy [StateMachineGamer] []
    (getInitialStateMachine []
      (ProverStateMachine.))

    (stateMachineSelectMove [timeout]
      (println "****************************************")
      (println "Making a move for" (.getRole this))
      (println "STATE:" (.getCurrentState this))
      (let [state-machine (.getStateMachine this)
            current-state (.getCurrentState this)
            role          (.getRole this)
            _ (reset! counts 0)
            move (best-move state-machine current-state role)]
        (println "SELECTING" role move "after" @counts "nodes")
        move))

    (stateMachineMetaGame [timeout]
      (println "SampleClojureGamer metagame called"))

    (stateMachineAbort []
      (println "SampleClojureGamer abort called"))

    (stateMachineStop []
      (println "SampleClojureGamer stop called"))))


