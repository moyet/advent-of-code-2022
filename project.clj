(defproject advent-of-code-2022 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.0"]
                 [hato "0.9.0"]
                 [clj-http "3.12.3"]
                 [pandect "1.0.2"]
                 ]
  :main ^:skip-aot advent-of-code-2022.core/main
  :repl-options {:init-ns advent-of-code-2022.core})
