(defproject ggp-clj "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :source-paths ["src_clj"]
  :resource-paths ["bin"
                   "bin/external/Guava/*"
                   "bin/external/Junit/*"
                   "bin/external/reflections/*"
                   "bin/external/javassist/*"]


  :aliases {"kiosk"
            ["trampoline" "run" "-m" "org.ggp.base.apps.kiosk.Kiosk"]

            "minmax"
            ["trampoline" "run" "-m" "org.ggp.base.player.GamePlayer"
             "9147"
             "org.ggp.base.player.gamer.clojure.stubs.NormanWeek5Player"]

            "alphabeta"
            ["trampoline" "run" "-m" "org.ggp.base.player.GamePlayer"
             "9147"
             "org.ggp.base.player.gamer.clojure.stubs.NormanWeek5PlayerAlphaBeta"]

            "w6p1"
            ["trampoline" "run" "-m" "org.ggp.base.player.GamePlayer"
             "9147"
             "org.ggp.base.player.gamer.clojure.stubs.NormanWeek6Player1"]})


