(defproject advent-code "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot advent-code.core
  :target-path "target/%s"
  :aliases {"rebel"     ["trampoline" "run" "-m" "rebel-readline.main"]}
  :profiles {:dev {:dependencies [[com.bhauman/rebel-readline "0.1.4"]
                                  [org.clojure/tools.namespace "1.0.0"]]
                   :source-paths ["dev"]}
             :uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
