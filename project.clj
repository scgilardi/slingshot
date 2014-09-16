(defproject slingshot "0.10.4-SNAPSHOT"
  :description "Enhanced throw, try, leveraging Clojure's capabilities"
  :url "https://github.com/scgilardi/slingshot"
  :license {:name "Eclipse Public License 1.0"}
  :dev-dependencies [[org.clojure/clojure "1.7.0-alpha2"]]
  :java-source-path "java/src"
  :java-source-paths ["java/src"]
  :global-vars {*warn-on-reflection* true}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.7.0-alpha2"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0-alpha2"]]}}
  :aliases {"all" ["with-profile" "1.4:1.5:1.6:1.7"]})
