(defproject mr_challenjer "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [criterium "0.4.3"]]

  :jvm-opts ^:replace ["-server"
                       ;; Uncomment this to enable assertions. Turn off during performance tests.
                       "-ea"
                       "-Dcom.sun.management.jmxremote"
                       "-Dcom.sun.management.jmxremote.ssl=false"
                       "-Dcom.sun.management.jmxremote.authenticate=false"
                       "-Dcom.sun.management.jmxremote.port=1098"]


  :profiles
  {:dev {:source-paths ["dev" "src"]
         :dependencies [[org.clojure/tools.namespace "0.2.4"]
                        [org.clojars.gjahad/debug-repl "0.3.3"]]}})
