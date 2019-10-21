(defproject slingshot "0.12.2"
  :description "Enhanced throw, try, leveraging Clojure's capabilities"
  :url "https://github.com/scgilardi/slingshot"
  :license {:name "Eclipse Public License 1.0"
            :url "https://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :deploy-repositories [["releases" :clojars]]
  :global-vars {*warn-on-reflection* true}
  :dependencies [[org.clojure/clojurescript "1.9.93" ]]
  :profiles {:dev {:dependencies [[org.clojure/clojure       "1.8.0"  ]
                                  [org.clojure/clojurescript "1.9.93" ]
                                  [org.clojure/core.async    "0.2.385"] ; To be explicit
                                  [doo                       "0.1.7"  ]] ; CLJS test
                   :resource-paths ["target/cljs/"]
                   :plugins      [[com.jakemccrary/lein-test-refresh "0.16.0"]
                                  [lein-cljsbuild                    "1.1.3" 
                                    :exclusions [org.clojure/clojurescript]]
                                  [lein-doo                          "0.1.7" 
                                    :exclusions [org.clojure/clojurescript]]
                                  [lein-figwheel "0.5.3-2"
                                    :exclusions [org.clojure/clojure]]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}}
  :aliases {"all" ["with-profile" #_"1.4:1.5:1.6:1.7:1.8" "1.7:1.8"] ; for CLJC
            "test:clj"      ["test"]
            "test:cljs"     ["doo" "phantom" "dev" "once"]
            "autotest:cljs" ["doo" "phantom" "dev"]
            "autotest:clj"  ["test-refresh"]}
  :clean-targets ^{:protect false} ["out" "target/cljs"]
  :doo {:build "dev"}
  :cljsbuild
    {:builds
      [{:id "dev"
        :source-paths ["src" "test"]
        :compiler {:main                 "slingshot.all-tests"
                   :asset-path           "js/compiled/out"
                   :output-to            "target/cljs/dev/slingshot.js"
                   :output-dir           "target/cljs/dev"
                   :source-map-timestamp true
                   :optimizations        :whitespace}}
       {:id           "figwheel"
        :source-paths ["src" "test"]
        :figwheel     true
        :compiler {:main                 "slingshot.all-tests"
                   :asset-path           "compiled"
                   :output-to            "target/cljs/figwheel/compiled/slingshot.js"
                   :output-dir           "target/cljs/figwheel/compiled"
                   :source-map-timestamp true
                   :optimizations        :none}}]}
  :figwheel {:http-server-root "figwheel"})
