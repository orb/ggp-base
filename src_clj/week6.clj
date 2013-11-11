(ns gamer_namespace
  (:require [ggp.util :refer :all]
            [ggp.player :as player])
  (:import[org.ggp.base.player.gamer.statemachine StateMachineGamer]
           [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]))

;; Implement a bounded-depth search player, i.e. a heuristic search
;; player with an evaluation function that returns actual rewards on
;; terminal states and 0 for all other states. Use a depth of at least
;; 2.

(defn hit-max-depth [game-state]
  (>= (:depth game-state) (:max-depth game-state)))

(defn call-heuristic [game-state]
  (let [heuristic (:heuristic game-state)]
    (heuristic game-state)))

(defn dfs-terminal [game-state]
  (log game-state "TERMINAL" (call-heuristic game-state))
  (assoc game-state :score (call-heuristic game-state)))

(defn select-role [game-state]
  (let [all-roles (.getRoles (:game game-state))
        needs-move (fn [role]
                     (not (contains? (:actions game-state) role)))]
    (first (filter needs-move all-roles))))

(declare bounded-dfs)

(defn dfs-explore [game-state]
  (let [role (select-role game-state)]
    (log game-state "EXPLORE" role (= role (:role game-state)))
    (let [actions (legal-moves-for game-state role)
          next-level (doall
                      (for [action actions
                            :let [make-move (move game-state role action)
                                  searched (bounded-dfs make-move)]]
                        [(:score searched) action]))

          _ (log game-state "NEXT" next-level)
          minormax (if (= role (:role game-state)) max-key min-key)
          [score action] (apply minormax first next-level)
          _ (log game-state "chosing[" score "]" action "using" minormax)]
      (-> game-state
          (assoc :score score)
          (assoc :score-action action)))))

(defn bounded-dfs [game-state]
  (bump! game-state)
  (log game-state "DFS"
       (str (:depth game-state) "/" (:max-depth game-state))
       (str "[" (terminal-score game-state) "]"))
  (cond
   (terminal? game-state)
   (dfs-terminal game-state) ;; really just should call goal, not heuristic?

   (hit-max-depth game-state)
   (dfs-terminal game-state)

   :else
   (dfs-explore game-state)))

(defn heuristic1 [game-state]
  (or (terminal-score game-state) 0))

(defn heuristic2 [game-state]
  (.getGoal (:game game-state)
            (:state game-state)
            (:role game-state)))


(defn week6-player1 []
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
                           :depth 0
                           :print-depth 2
                           :max-depth 4
                           :heuristic heuristic2
                           :count counter
                           :role (.getRole this)}
            scored (bounded-dfs initial-state)
            _ (println "!!! legal moves are" (my-legal-moves initial-state))
            move (:score-action scored)] ;; single player
        (println "SELECTING" (:role initial-state) move "with score" (:score scored)
                 "after" @counter "nodes")
        move))

    (stateMachineMetaGame [timeout]
      (println "SampleClojureGamer metagame called"))

    (stateMachineAbort []
      (println "SampleClojureGamer abort called"))

    (stateMachineStop []
      (println "SampleClojureGamer stop called"))))

(println "XXX loading w6p1")
;; ----------------------------------------
;; this still needs some work..
#_(defn start []
    (player/start (week6-player1)))
