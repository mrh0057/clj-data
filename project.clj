(defproject clj-data "0.0.1-SNAPSHOT"
  :description "A clojure library for managing data for data minning and stats purposes."
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [ujmp-complete/ujmp-complete "2.5"]]
  :dev-dependencies [[lein-midje "1.0.3"]
                     [swank-clojure "1.3.1"]
                     [midje "1.1.1" :exclusions [org.clojure/clojure
                                                 org.clojure.contrib/core]]])
