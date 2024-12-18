(defproject adventofcode "0.1.0-SNAPSHOT"
  :description "My solution to advent of code"
  :url "http://example.com/FIXME"
  :source-paths ["src" "dev"]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [net.mikera/imagez "0.12.0"]
                 [clojure2d "1.4.3"]
                 [midje "1.9.0"]
                 [djblue/portal "0.50.0"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [automat "0.2.4"]
                 [net.clojars.jordibc/geometric-algebra "0.9.2"]
                 [shams/priority-queue "0.1.2"]
                 ]
  :plugins [[lein-midje "3.1.3"]])
