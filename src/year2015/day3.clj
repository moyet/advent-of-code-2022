(ns year2015.day3
  (:require [helpers.help :as help]
            [clojure.string :as str]
            )
  )


(def input (help/get-data-from-this-day 2015 3))

(def start {:pos [0 0]
            :visited #{}
            :vektor (vec input)
            })

(defn moving [{:keys [pos visited vektor]}]
  (let [karakter (first vektor)
        [x y] pos
        new-pos (cond
                  (= karakter \v) [x (dec y)]
                  (= karakter \^) [x (inc y)]
                  (= karakter \<) [(dec x) y]
                  (= karakter \>) [(inc x) y]
                  :else [x y])
        new-visited (conj visited new-pos)]

    {:pos new-pos
     :vektor (rest vektor)
     :visited new-visited}))

(defn moving-again [{:keys [pos-santa pos-rob visited vektor]}]
  (let [[ks kr] (first vektor)
        [x y] pos-santa
        [z w] pos-rob
        new-pos-santa (cond
                        (= ks \v) [x (dec y)]
                        (= ks \^) [x (inc y)]
                        (= ks \<) [(dec x) y]
                        (= ks \>) [(inc x) y]
                        :else [x y])
        new-pos-rob (cond
                      (= kr \v) [z (dec w)]
                      (= kr \^) [z (inc w)]
                      (= kr \<) [(dec z) w]
                      (= kr \>) [(inc z) w]
                      :else [z w])

        new-visited (conj visited new-pos-santa new-pos-rob)]

    {:pos-santa new-pos-santa
     :pos-rob new-pos-rob
     :vektor (rest vektor)
     :visited new-visited}))


(defn day3-1
  []
  (loop [res start ]
    (if (= 0 (count (:vektor res)))
      res
      (recur (moving res))
      )
    )
  )

(defn day3-2
  []
  (let [partitioned (partition 2 input)]
    (loop
      [res {:vektor partitioned
            :pos-santa [0 0]
            :pos-rob [0 0]
            :visited #{}
            }]
      (if (= 0 (count (:vektor res)))
        res
        (recur (moving-again res))))))
