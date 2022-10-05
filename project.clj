(defproject org.clojars.tapochqa/lufs "0.3.0"
  :description "LUFS meter"

  :url "https://github.com/tapochqa/lufs"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [ [org.clojure/clojure "1.11.0"]
                  [prismatic/hiphip "0.2.1"]]
  :main ^:skip-aot lufs-clj.core
  :target-path "target/%s"
  :uberjar-name "lufs-clj.jar"
  :jvm-opts ^:replace []
  :profiles { :uberjar {
                    :aot :all
                    :global-vars {*unchecked-math* :true}}
              :dev {
                    :global-vars {*unchecked-math* :true}}})
