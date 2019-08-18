(defproject adventofcode "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.trace "0.7.10"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [com.rpl/specter "1.1.0"]
                 [clj-time "0.15.2"]
                 
                 [org.clojure/test.check "0.9.0"]
                 [plumula/mimolette "0.2.1"]
                 [org.clojure/math.combinatorics "0.1.4"]]

  :source-paths ["src/main" "src/workbench" "src/spec"]
  :test-paths ["test/main"])
