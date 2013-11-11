(ns ggp.repository
  (:import [org.ggp.base.player.gamer.statemachine StateMachineGamer]
           [org.ggp.base.util.statemachine.implementation.prover ProverStateMachine]
           [org.ggp.base.util.game GameRepository]))


(defn start-local-repository []
  (org.ggp.base.util.game.LocalGameRepository.))
(defn local-repository []
  (org.ggp.base.util.game.CloudGameRepository. "localhost:9140"))

(def repository (GameRepository/getDefaultRepository))
(comment
  (.getGameKeys repository))

(defonce buttons-game (.getGame repository "buttons"))
(defonce ttt-game (.getGame repository "tictactoe2"))

(defn state-machine [game]
  (doto (ProverStateMachine.)
    (.initialize (.getRules game))))


(defn local-game [name]
  (state-machine (.getGame (local-repository) name)))

(def buttons (state-machine buttons-game))
(def ttt (state-machine ttt-game))

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




