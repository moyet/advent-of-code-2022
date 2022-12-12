(ns advent-of-code-2022.day8
  (:require [clojure.string :as str]
            ))

(defn day8-question1
  []
  (let [visible (atom 0)
        input (->>
                "resources/day8.data"
                slurp
                (str/split-lines)
                (map (fn [line] (map #(-> %
                                          str
                                          Integer/parseInt) line))))
        input-atom (atom (vec (map vec input)))
        size (count input)]

    (letfn [
            (get-height [x y] (-> input
                                  (nth x)
                                  (nth y)))
            (visible? [x y]
              (if (or (= x 0) (= x (dec size)) (= y 0) (= y (dec size)))
                true
                (let [height (get-height x y)
                      x-heights (nth input x)
                      y-heights (->> input
                                     (map #(nth % y)))

                      lower-x (take y x-heights)
                      lower-y (take x y-heights)
                      upper-x (take-last (dec (- size y)) x-heights)
                      upper-y (take-last (dec (- size x)) y-heights)
                      ]
                  [x-heights y-heights]
                  (or
                    (> height (apply max lower-x))
                    (> height (apply max upper-x))
                    (> height (apply max lower-y))
                    (> height (apply max upper-y))
                    )
                  )
                ))
            ]
      (doseq [x (range size)]
        (doseq [y (range size)]
          (if (visible? x y)
            (do (swap! visible inc)
                (swap! input-atom assoc-in [x y] true)
                )
            (swap! input-atom assoc-in [x y] false)
            )
          )
        ))
    @input-atom
    @visible
    )
  )

(defn index-of-coll
  ([coll elm]
   (index-of-coll coll elm 0))
  ([[first & rest :as coll] elm idx]
   (cond (empty? coll) -1
         (= first elm) idx
         :else (recur rest elm (inc idx)))))

(defn day8-question2
  []
  (let [input (->>
                "resources/day8.data"
                slurp
                (str/split-lines)
                (map (fn [line] (map #(-> %
                                          str
                                          Integer/parseInt) line))))
        input-atom (atom (vec (map vec input)))
        size (count input)]

    (letfn [
            (get-height [x y] (-> input
                                  (nth x)
                                  (nth y)))
            (calculate-score [x y]
              (if (or (= x 0) (= x (dec size)) (= y 0) (= y (dec size)))
                0
                (let [height (get-height x y)
                      x-heights (nth input x)
                      y-heights (->> input
                                     (map #(nth % y)))

                      west (take y x-heights)
                      north (take x y-heights)
                      east (take-last (dec (- size y)) x-heights)
                      south (take-last (dec (- size x)) y-heights)

                      north-score (let [mapped (map #(>= % height) (reverse north))]
                                    (println "north mapped " mapped)
                                    (if (reduce #(or %1 %2) mapped)
                                      (inc (index-of-coll mapped true))
                                      (count north)
                                      )
                                    )
                      south-score (let [mapped (map #(>= % height) south)]
                                    (println "south mapped " mapped)
                                    (if (reduce #(or %1 %2) mapped)
                                      (inc (index-of-coll mapped true))
                                      (count south)
                                      )
                                    )
                      east-score (let [mapped (map #(>= % height) east)
                                       ]
                                   (println "east mapped " mapped)
                                   (if (reduce #(or %1 %2) mapped)
                                     (inc (index-of-coll mapped true))
                                     (count east)
                                     )
                                   )
                      west-score (let [mapped (map #(>= % height) (reverse west))]
                                   (println "west mapped " mapped)
                                   (if (reduce #(or %1 %2) mapped)
                                     (inc (index-of-coll mapped true))
                                     (count west)
                                     )
                                   )
                      ]

                  (* north-score south-score east-score west-score))))]

      (doseq [x (range size)]
        (doseq [y (range size)]
          (swap! input-atom assoc-in [x y] (calculate-score x y))
          )
        )
      (->> @input-atom
           flatten
           (apply max))
      )

    )
  )
