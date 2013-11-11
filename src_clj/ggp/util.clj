(ns ggp.util)

;; ----------------------------------------
;; logging

(defn spaces [n] (apply str (take n (repeat " "))))
(defn log! [game-state & args]
  (apply println (spaces (:depth game-state)) (:depth game-state) args))

(defn log [game-state & args]
  (let [depth (or (:depth game-state) 0)
        print-depth (or (:print-depth game-state) 4)]
    (when (< depth print-depth)
      (apply log! game-state args))))


(defmacro logging-depth [[game-state message] & body]
  `(let [val# (do ~@body)]
     (log ~game-state ~message val#)
     val#))

;; ----------------------------------------
;; move counter

(defn bump! [game-state]
  (if (zero? (mod (swap! (:count game-state) inc) 10000))
    (println "COUNT" @(:count game-state))))


;; ----------------------------------------
;; role management

(defn find-opponent [game role]
  (first (filter #(not (= role %)) (.getRoles game))))

;; ----------------------------------------
;; scoring

(defn terminal? [game-state]
  (.isTerminal (:game game-state) (:state game-state)))

(defn terminal-score [game-state]
  (when (terminal? game-state)
    (.getGoal (:game game-state)
              (:state game-state)
              (:role game-state))))

;; ----------------------------------------
;; legal moves

(defn legal-moves-for [game-state role]
  (.getLegalMoves (:game game-state)
                  (:state game-state)
                  role))

(defn my-legal-moves [game-state]
  (.getLegalMoves (:game game-state)
                  (:state game-state)
                  (:role game-state)))

(defn opponent-legal-moves [game-state]
  (.getLegalMoves (:game game-state)
                  (:state game-state)
                  (:opponent-role game-state)))


;; ----------------------------------------
;; move helpers

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
    (if (= (count (:actions state))
           (count (.getRoles (:game game-state))))
      (next-state state)
      state)))

