(ns minimax)


(defmulti print-node type)
(defprotocol IAlphaBeta
  (search [node alpha beta]))

;; alpha = best max path so far
;; beta = best min path so far

(defrecord LeafNode [score]
  IAlphaBeta
  (search [this alpha beta]
    (println "SEARCHLEAF[" score "]" alpha beta (print-node this))
    score))


(defrecord MaxNode [children]
  IAlphaBeta
  (search [this alpha beta]
    (println "SEARCHMAX" alpha beta (print-node this))
    (let [best
          (min beta
               (reduce (fn [alpha child]
                         (if (>= alpha beta)
                           beta
                           (max alpha (search child alpha beta))))
                       alpha
                       children))]
      (println "--MAX ->" best)
      best)))

(defrecord MinNode [children]
  IAlphaBeta
  (search [this alpha beta]
    (println "SEARCHMIN" alpha beta (print-node this))
    (let [best
          (max alpha
                     (reduce (fn [beta child]
                               (if (<= beta alpha)
                                 alpha
                                 (min beta (search child alpha beta))))
                             beta
                             children))]
      (println "--min ->" best)
      best)))

(defmethod print-node LeafNode [l] (:score l))
(defmethod print-node MaxNode [n] (str "MAX["
                                       (clojure.string/join ":" (map print-node (:children n)))
                                       "]"))
(defmethod print-node MinNode [n] (str "MIN["
                                       (clojure.string/join ":" (map print-node (:children n)))
                                       "]"))


(defn leaves [nodes]
  (map #(if (number? %)
          (LeafNode. %)
          %)
       nodes))

(defn alpha [& nodes]
  (MaxNode. (leaves nodes)))

(defn beta [& nodes]
  (MinNode. (leaves nodes)))


(def t
  (alpha  (beta (alpha  (beta 4 3)
                        (beta 2 1))
                (alpha  (beta 8 7)
                        (beta 6 5)))
          (beta (alpha  (beta 12 11)
                        (beta 10 9))
                (alpha  (beta 16 15)
                        (beta 14 13)))))


(def s1 (alpha  (beta 4 3)
                (beta 2 1)))

(def s2 (beta (alpha  (beta 4 3)
                        (beta 2 1))
                (alpha  (beta 8 7)
                        (beta 6 5))))

