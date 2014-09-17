(defproject slingshot "0.11.0"
  :description "Enhanced throw, try, leveraging Clojure's capabilities"
  :url "https://github.com/scgilardi/slingshot"
  :scm {:name "git"
        :url "https://github.com/scgilardi/slingshot"}
  :license {:name "Eclipse Public License 1.0"
            :url "https://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :deploy-repositories [["clojars" {:creds :gpg}]]
  :signing {:gpg-key "51D2FEA1"}
  :global-vars {*warn-on-reflection* true}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.7.0-alpha2"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0-alpha2"]]}}
  :aliases {"all" ["with-profile" "1.4:1.5:1.6:1.7"]})
