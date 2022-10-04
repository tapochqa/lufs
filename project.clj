(defproject org.clojars.tapochqa/lufs "0.1.0"
  :description "LUFS counter"

  :url "https://github.com/tapochqa/lufs"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.0"]]
  :main ^:skip-aot lufs-clj.core
  :target-path "target/%s"
  :profiles { :uberjar {
                    :aot :all
                    :global-vars {*unchecked-math* :true}}
              :dev {
                    :global-vars {*unchecked-math* :true}}})
