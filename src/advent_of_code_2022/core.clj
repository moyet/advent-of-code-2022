(ns advent-of-code-2022.core
  (:require [clojure.string :as str])
  )

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn sum-it
  [x]
  (->>
    x
    (map #(Integer/parseInt %))
    (apply +)
    )
  )

(defn day1
  []
  (let [
        input (->>
                "resources/day1.data"
                slurp
                str/split-lines
                (partition-by #(= "" %))
                (filter #(not= '("") %))
                (map sum-it)
                )
        question1 (apply max input)
        question2 (->> input
             sort
             reverse
             (partition 3)
             first
             (apply +)
             )

        ]
    [question1 question2] ))