(ns year2024.day3
  (:require [helpers.help :as help]
            [clojure.walk :as w]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math]
            ))

(defonce input (->> (help/get-data-from-this-day 2024 3)))


(defn q1 [input]
  (->> input (re-seq #"mul\((\d+),(\d+)\)")
       (map (fn [[a b c]]
              (* (Integer/parseInt b) (Integer/parseInt c))))
       (apply +)))


(defn q2 [input]
  (let [do? (atom true)
        c (atom 0)]
  (->> input
       (re-seq #"(mul\((\d+),(\d+)\)|do\(\)|don\'t\(\))")
       (w/walk (fn [[_ message n1 n2]]
                 (cond
                   (= message "do()")  (reset! do? true)
                   (= message "don't()") (reset! do? false)
                   @do? (swap! c + (Integer/parseInt n1) (Integer/parseInt n2)))
                 ) identity)
       )
  @c
  ))
