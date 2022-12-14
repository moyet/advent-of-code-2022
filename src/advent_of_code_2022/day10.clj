(ns advent-of-code-2022.day10
  (:require [clojure.string :as str]
            ))

(def signal (atom {}))
(def value (atom 1))
(def tick (atom 1))
(def screen (atom []))

(defn day10-question1 []
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


(defn day10-question2 []
  (reset! screen [])
  (let [
        input (->>
                "resources/day10.data"
                slurp
                str/split-lines
                (map #(str/split % #" "))
                )
        importants-ticks [20 60 100 140 180 220]
        ]
    (swap! signal assoc @tick {:value @value :signal (* @tick @value)})

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
    )
  (for [i (range 1 242)]
    (let [val (get (@signal i) :value)
          pos (mod i 40)
          ]
      (if (contains? #{-1 0 1} (- (mod pos 40) val))
        (swap! screen conj \#)
        (swap! screen conj \.)
        )

      (let [
            parts (partition 40 @screen)
            sss (map str/join parts)
            output (str/join "\n" sss)
              ]
        (apply str output)
        )
      )
    )
  )