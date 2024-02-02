(defproject com.cleancoder/functor "0.0.1-SNAPSHOT"
  :description "Functor macro"
  :url "https://github.com/unclebob/functor"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :profiles {:dev {:dependencies [[speclj "3.4.5"]]}}
  :plugins [[speclj "3.4.5"]]
  :test-paths ["spec"])
