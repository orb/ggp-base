(ns gamer_namespace
  (:import [org.ggp.base.player.gamer.statemachine StateMachineGamer]
           [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]))

(defn search [game state role]
  (if (.isTerminal game state)
    [nil (.getGoal game state role)]
    (let [paths
          (for [move (.getLegalMoves game state role)]
            (let [[moves score] (search game (.getNextState game state [move]) role)]
              [(cons move moves) score]))]
      (apply max-key second paths))))

(defn week4 []
  (proxy [StateMachineGamer] []
    ;; NOTE: the implicit 'this symbol is bound to the local class.

    (getInitialStateMachine []
      (ProverStateMachine.))

    (stateMachineSelectMove [timeout]
      (println "Making a move!")
      (let [state-machine (.getStateMachine this)
            current-state (.getCurrentState this)
            role          (.getRole this)
            [[move _] score] (search state-machine current-state role)]
        (println "move" move "expected score" score)
        move))

    (stateMachineMetaGame [timeout]
      (println "SampleClojureGamer metagame called"))

    (stateMachineAbort []
      (println "SampleClojureGamer abort called"))

    (stateMachineStop []
      (println "SampleClojureGamer stop called"))))
