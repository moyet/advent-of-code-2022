(ns year2015.day1
  (:require [helpers.help :as help]))


(def input (help/get-data-from-this-day 2015 1))

(defn question1
  []
  (let [
        start-p (->> input
                     vec
                     (filter #(= % \())
                     count)

        slut-p  (->> input
                     vec
                     (filter #(= % \)))
                     count)]

    (- start-p slut-p)))

(defn question2
  []
  (loop
    [floor 0
     step 0
     v (-> input vec)]

    (let [f (first v)
          r (rest v)
          f (if (= f \() (inc floor) (dec floor))
          s (inc step)]

      (if (= f -1)
        s
        (recur f s r)))))
