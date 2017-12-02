(defproject aoc2017 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.rpl/specter "1.0.5"]
                 [datascript "0.15.5"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [prismatic/plumbing "0.4.4"]
                 [vvvvalvalval/supdate "0.2.2"]
                 [vvvvalvalval/scope-capture "0.1.4"]
                 [vvvvalvalval/scope-capture-nrepl "0.2.0"]
                 [org.clojure/core.logic "0.8.11"]
                 [com.cerner/clara-rules "0.16.1"]]
  :repl-options {:init-ns aoc2017.core
                 :nrepl-middleware [sc.nrepl.middleware/wrap-letsc]})


