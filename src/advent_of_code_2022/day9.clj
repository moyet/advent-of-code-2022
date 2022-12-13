(ns advent-of-code-2022.day9
  (:require [clojure.string :as str]
            ))


(def tail-positions (atom []))
(def head-position (atom {:x 0 :y 0}))
(def tail-position (atom {:x 0 :y 0}))
(def positions (atom []))


(defn tail-follow []
  (let [x-diff (- (@head-position :x) (@tail-position :x))
        y-diff (- (@head-position :y) (@tail-position :y))]

    (case [x-diff y-diff]
      [0 2] (swap! tail-position update-in [:y] inc)
      [2 0] (swap! tail-position update-in [:x] inc)
      [0 -2] (swap! tail-position update-in [:y] dec)
      [-2 0] (swap! tail-position update-in [:x] dec)

      ([1 2] [2 1]) (do
                      (swap! tail-position update-in [:x] inc)
                      (swap! tail-position update-in [:y] inc)
                      )

      ([1 -2] [2 -1]) (do
                        (swap! tail-position update-in [:y] dec)
                        (swap! tail-position update-in [:x] inc)
                        )

      ([-2 1] [-1 2]) (do
                         (swap! tail-position update-in [:y] inc)
                         (swap! tail-position update-in [:x] dec))
      ([-2 -1] [-1 -2]) (do
                          (swap! tail-position update-in [:y] dec)
                          (swap! tail-position update-in [:x] dec)
                          )

      ([0 0] [1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 -1] [1 -1] [-1 1]) (println "Nothing will happen 0 0 ")
      )

    ;; lets update tail-posistion
    (swap! tail-positions conj @tail-position)
    (swap! positions conj @head-position)

    )
  )



(defn day9-question1
  []
  (let [input (->>
                "resources/day9.data"
                slurp
                str/split-lines
                (map #(str/split % #" "))
                )]
    (doseq [[direction length] input]
      (let [length (Integer/parseInt length)]
        (dotimes [l length]
          (case direction
            "R" (swap! head-position update-in [:x] inc)
            "L" (swap! head-position update-in [:x] dec)
            "U" (swap! head-position update-in [:y] inc)
            "D" (swap! head-position update-in [:y] dec)
            )
          (tail-follow)

          )
        )

      )
    (-> @tail-positions
        set
        count)
    )
  )