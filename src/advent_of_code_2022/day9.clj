(ns advent-of-code-2022.day9
  (:require [clojure.string :as str]
            ))


(def tail-positions (atom []))
(def head-position (atom {:x 0 :y 0}))
(def tail-position (atom {:x 0 :y 0}))
(def positions (atom  [{:x 0 :y 0}
                       {:x 0 :y 0}
                       {:x 0 :y 0}
                       {:x 0 :y 0}
                       {:x 0 :y 0}
                       {:x 0 :y 0}
                       {:x 0 :y 0}
                       {:x 0 :y 0}
                       {:x 0 :y 0}]))


(defn tail-follow [x-diff y-diff]
  (let [x-diffs (- (@head-position :x) (@tail-position :x))
        y-diffs (- (@head-position :y) (@tail-position :y))]

    (case [x-diff y-diff]
      [0 2] [0 1]
      [2 0] [1 0]
      [0 -2] [0 -1]
      [-2 0] [-1 0]
      ([1 2] [2 1] [2 2]) [1 1]
      ([1 -2] [2 -1] [2 -2]) [1 -1]
      ([-2 1] [-1 2] [-2 2] ) [-1 1]
      ([-2 -1] [-1 -2] [-2 -2]) [-1 -1]
      ([0 0] [1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 -1] [1 -1] [-1 1]) [0 0]
      (println "Error " x-diff y-diff)
      )
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
          (let [[x y] (tail-follow (- (@head-position :x) (@tail-position :x))
                                   (- (@head-position :y) (@tail-position :y)))
                ]
            (swap! tail-position update-in [:x] #(+ % x))
            (swap! tail-position update-in [:y] #(+ % y))

            (swap! tail-positions conj @tail-position)
            )
          )
        )
      )
    (-> @tail-positions
        set
        count)
    )
  )

(defn day9-question2
  []
  (let [input (->>
                "resources/day9.data"
                slurp
                str/split-lines
                (map #(str/split % #" "))
                )]

    (doseq [[direction length] input]
      (let [length (Integer/parseInt length)]
        (dotimes [_ length]
          (case direction
            "R" (swap! head-position update-in [:x] inc)
            "L" (swap! head-position update-in [:x] dec)
            "U" (swap! head-position update-in [:y] inc)
            "D" (swap! head-position update-in [:y] dec))

          (dotimes [pos 9]
            (try (let [body-pos (nth @positions pos)
              previous-pos (if (= pos 0)
                             @head-position
                             (nth @positions (dec pos))
                             )

              [x y] (tail-follow (- (previous-pos :x) (body-pos :x))
                                 (- (previous-pos :y) (body-pos :y)))
              ]

              (swap! positions update-in [pos :x] #(+ % x))
              (swap! positions update-in [pos :y] #(+ % y))
              )
               (catch Exception e
                 (println e)
                 )  ))

          (swap! tail-positions conj (nth @positions 8))
          )
        )
      )
    (-> @tail-positions
        set
        count)
    )
  )