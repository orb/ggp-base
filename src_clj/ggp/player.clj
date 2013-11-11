(ns ggp.player
  (:import [org.ggp.base.player GamePlayer]))


(defn start
  ([player]
     (start 9147 player))

  ([port player]
     (println "player is" player)
     (doto (GamePlayer. port player)
       (.start))))



