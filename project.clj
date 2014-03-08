(defproject slingshot "0.10.4-SNAPSHOT"
  :description "Enhanced throw, try, leveraging Clojure's capabilities"
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :java-source-path "java/src"
  :java-source-paths ["java/src"]
  :warn-on-reflection true
  :url "https://github.com/scgilardi/slingshot"
  :profiles {:1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0-beta2"]]}}
  :aliases {"all" ["with-profile" "1.4:1.6"]})
