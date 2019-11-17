(defproject chess "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.520"
                  :exclusions [com.google.javascript/closure-compiler-unshaded
                               org.clojure/google-closure-library]]
                 [thheller/shadow-cljs "2.8.62"]
                 [reagent "0.8.1"]
                 [re-frame "0.10.9"]
                 [http-kit "2.3.0"]
                 [compojure "1.6.1"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/core.match "0.3.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.6"]]

  :plugins []

  :min-lein-version "2.5.3"

  :source-paths ["src/clj" "src/cljs" "src/cljc"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :aliases
  {"dev"  ["with-profile" "dev" "run" "-m" "shadow.cljs.devtools.cli" "watch" "app-dev"]
   "prod" ["with-profile" "prod" "run" "-m" "shadow.cljs.devtools.cli" "release" "app-prod"]
   "serv" ["run" "-m" "chess.serv"]}

  :profiles
  {:dev {:dependencies [[binaryage/devtools "0.9.10"]]}
   :prod {}})
