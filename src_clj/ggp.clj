(ns ggp
  (:import [org.ggp.base.player.gamer.statemachine StateMachineGamer]
           [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]
           [org.ggp.base.util.game GameRepository]))


(def repository (GameRepository/getDefaultRepository))
(defonce buttons-game (.getGame repository "buttons"))

(defn state-machine [game]
  (doto (ProverStateMachine.)
    (.initialize (.getRules game))))

(def buttons (state-machine buttons-game))

(defn search [game state role]
     (if (.isTerminal game state)
       [nil (.getGoal game state role)]
       (let [paths
             (doall
              (for [move (.getLegalMoves game state role)]
                (let [[moves score] (search game (.getNextState game state [move]) role)]
                  [(cons move moves) score])))]
         (apply max-key second paths))))


(defn solve-buttons []
  (let [role (first (.getRoles buttons))
        state (.getInitialState buttons)]
    (search buttons state role)))


(def final-state
  (.performDepthCharge (.getStateMachine this)
                       (first (.get (vals nextStates) 0))
                       depth))
