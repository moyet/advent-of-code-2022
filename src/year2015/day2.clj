(ns year2015.day2
  (:require [helpers.help :as help]
            [clojure.string :as str]
            )
  )

(def input (help/get-data-from-this-day 2015 2))

(defn- calc-something
  [[l w h]]
  (let [s1 (* 2 l w)
        s2 (* 2 w h)
        s3 (* 2 h l)
        m (/ (min s1 s2 s3) 2)]
    (+ s1 s2 s3 m)))

(defn calc-something-else
  [l]
  (let [rz (apply * l)
        s (->>
            l
            sort
            (take 2)
            (apply +)
            (* 2)
            )
        ]
    (+ s rz)
    )
  )

(defn question-1 []
  (let [input (->> input
                   str/split-lines
                   (map (fn [l]
                          (->> (str/split l #"x")
                               (map #(Integer/parseInt %))
                               calc-something
                               )))
                   (apply +)
                   )]
    input
    )
  )

(defn question-2
  []
  (let [input (->> input
                   str/split-lines
                   (map (fn [l]
                          (->> (str/split l #"x")
                               (map #(Integer/parseInt %))
                               vec
                               calc-something-else
                               )))
                   (apply +)
                   )]
    input
    )
  )
