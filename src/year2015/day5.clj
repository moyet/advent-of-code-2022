(ns year2015.day5
  (:require [helpers.help :as help]
            [clojure.string :as str])
  )


(def input (help/get-data-from-this-day 2015 5))

(defn day5-1
  []
  (let [vokaler #"[aeiou]"
        disallowed #"(ab)|(cd)|(pq)|(xy)"
        input (->> input
                   str/split-lines
                   (filter (fn [streng]
                             (->>
                               streng
                               (re-seq vokaler)
                               count
                               (<= 3))))
                   (filter (fn [streng]
                             (->> streng
                                  (re-find #"(.)\1"))))
                   (filter (fn [streng]
                             (->> streng
                                  (re-find disallowed)
                                  count
                                  (= 0))))
                   count)]
    input))

(defn day5-2
  []
  (let [input
        (->> input
             str/split-lines
             (filter (fn [strengt]
                       (->> strengt
                            (re-find #"(..).*\1"))))
             (filter (fn [strengt]
                       (->> strengt
                            (re-find #"(.).\1"))))
             count)]
    input))
