(ns advent-of-code-2022.day10
  (:require [clojure.string :as str]
            ))


(def signal (atom {}))
(def value (atom 1))
(def tick (atom 1))

(defn day10 []
  (let [
        input (->>
                "resources/day10.data"
                slurp
                str/split-lines
                (map #(str/split % #" "))
                )
        importants-ticks [20 60 100 140 180 220]
        ]
    (doseq [line input]

      (let [[operation change] line]
        (if (= operation "noop")
          (do
            (swap! tick inc)
            (swap! signal assoc @tick {:line line :value @value :signal (* @tick @value)})
            )
          (do
            (swap! tick inc)
            (swap! signal assoc @tick {:line line :value @value :signal (* @tick @value)})

            (swap! tick inc)
            (swap! value + (Integer/parseInt change))
            (swap! signal assoc @tick {:line line :value @value :signal (* @tick @value)})
            )
          )
        )
      )
    (apply + (map #(:signal (get @signal %)) importants-ticks))
    )
  )