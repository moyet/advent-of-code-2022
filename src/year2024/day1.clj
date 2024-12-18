(ns year2024.day1
  (:require [helpers.help :as help]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math]
            ))


(defonce input
         (->> (help/get-data-from-this-day 2024 1)
              str/split-lines
              ))


(defn q1
  [input]
  (let [firsts (map (fn [l]
                      (let [[f s] (str/split l #"\s+")]
                        (Integer/parseInt f))) input)
        seconds (map (fn [l]
                       (let [[f s] (str/split l #"\s+")]
                         (Integer/parseInt s))) input)
        zipped       (zipmap (sort firsts) (sort seconds))

        ]
    (apply + (map (fn [[f s]] (abs (- f s))) zipped))
    ))

(defn q2
  [input]
  (let [firsts (map (fn [l]
                      (let [[f s] (str/split l #"\s+")]
                        (Integer/parseInt f))) input)
        seconds (map (fn [l]
                       (let [[f s] (str/split l #"\s+")]
                         (Integer/parseInt s))) input)
        freqs   (frequencies seconds)]
    (->> firsts
         (map (fn [f]
                (* f (get (frequencies seconds) f 0))
                ))
         (apply +)

         )
    ))

