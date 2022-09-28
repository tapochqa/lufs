(defproject lufs-clj "0.1.0-SNAPSHOT"
  :description "LUFS counter"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.11.0"]
                 [kunstmusik/pink "0.4.1"]
                 [kunstmusik/diff-eq "0.1.2"]
                 [org.craigandera/dynne "0.4.1"]]

  :plugins [[lein-environ "1.1.0"]]

  :main ^:skip-aot lufs-bot-clj.core
  :target-path "target/%s"

  :profiles {:uberjar {:aot :all}})
