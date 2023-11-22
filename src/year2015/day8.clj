(ns year2015.day8
  (:require [helpers.help :as help]
            [clojure.string :as str]))

(def input (help/get-data-from-this-day 2015 8))

(defn clean-up
  [s]
  (-> s
      (str/replace #"\\x([0-9a-f]{2})" "X" )
      (str/replace #"\\\\" "Y")
      (str/replace #"\\\"" "Z")
       )
  )

(defn question1
  []
  (let [input (->> input
                   str/split-lines
                   first

                   )]
    input
    ))
