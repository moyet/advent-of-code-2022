(ns advent-of-code-2022.day15
  (:require [clojure.string :as str]
            [clojure.set :as set]
            )
  )

(def cave (atom {}))
(def min-x (atom 0))
(def max-x (atom 0))
(def min-y (atom 0))
(def max-y (atom 0))


(defn print-cave
  []
  (let [output (atom [])
        k (keys @cave)
        xs (sort (set (map first k)))
        ys (sort (set (map second k)))
        ]

    (doseq [y ys]
      (swap! output assoc y (str (apply str (map #(get @cave [% y]) xs)) "  " y))
      )
    (println (str/join "\n" (map str/join @output)))
    )
  )


(defn manhattan-distance
  [point-a point-b]
  (let [
        [sx sy] point-a
        [bx by] point-b
        distance (+
                   (Math/abs (- sx bx))
                   (Math/abs (- sy by))
                   )
        ]
    distance
    ))


(defn points-in-the-distance
  [point distance]
  (let [[px py] point
        xs (range @min-x (inc @max-x))
        ys (range @min-y (inc @max-y))
        xs (filter (fn [x] (< (Math/abs (- px x)) distance)) xs)
        ys (filter (fn [y] (< (Math/abs (- py y)) distance)) ys)
        ]
    (doseq [x xs]
      (doseq [y ys]
        (if (<= (manhattan-distance point [x y]) distance)
          (if (= \. (get @cave [x y]))
            (swap! cave assoc [x y] \#)
            )
          )
        )
      )
    )
  )



(defn pull-out
  [line]
  (let [[_ sx sy bx by] (re-matches #"Sensor at x=(\d+), y=(\d+): closest beacon is at x=([0-9-]+), y=([0-9-]+)" line)
        [sx sy bx by] (map #(Integer/parseInt %) [sx sy bx by])
        distance (manhattan-distance [sx sy] [bx by])
        ]
    [sx sy bx by distance]

    )
  )

(def alone (atom {}))
(def line-number 2000000)
(def test-file "resources/day15.data")
(def input (->>
             test-file
             slurp
             str/split-lines
             (map pull-out)
             ))

(def beacons (map (fn [line] [(nth line 2) (nth line 3)]) input))

(defn question1
  []
  (let [
        points (map (fn [n]
                      [[(first n) (second n)] (last n)]
                      ) input)

        xs (set (concat (map first input) (map #(nth % 2) input)))
        ys (set (concat (map second input) (map #(nth % 3) input)))
        ]

    (reset! min-x (apply min xs))
    (reset! max-x (apply max xs))
    (reset! min-y (apply min ys))
    (reset! max-y (apply max ys))

    (doseq [line points]
      (let [
            [x y] (first line)
            dista (second line)
            new-min-x (- x dista)
            new-max-x (+ x dista)
            new-min-y (- y dista)
            new-max-y (+ y dista)
            ]
        (swap! min-x min new-min-x)
        (swap! min-y min new-min-y)
        (swap! max-x max new-max-x)
        (swap! max-y max new-max-y)
        )
      )

    (let [
          xxx (range @min-x (inc @max-x))
          mapped (sort-by first (zipmap xxx (repeat line-number)))
          ]

      (doseq [my-point mapped]
        (doseq [point points]
          (let [local-point (first point)
                local-distance (second point)
                calculatede-distance (manhattan-distance my-point local-point)
                ]
            (if (<= calculatede-distance local-distance)
              (swap! alone assoc my-point calculatede-distance)
              )
            )

          #_(let [point-distance (last point)
                  distance (manhattan-distance my-point (first point))]
              (println "   " (first point) point-distance distance)
              (if (< distance point-distance)
                (do
                  #_(println "   Small enough " point-distance distance)
                  (swap! alone inc)
                  )
                (println "   too big " point-distance distance)
                )
              )
          )
        )
      mapped
      )
    (count (filter #(not (contains? (set beacons) (key %))) @alone))
    )
  )