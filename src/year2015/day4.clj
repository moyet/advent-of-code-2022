(ns year2015.day4
  (:require [pandect.algo.md5 :as m]))


(defn day4-1
  []
  (let [input "bgvyzdsv"]
    (loop
      [counter 0]
      (let [
            result (->> counter
                        (str input)
                        m/md5
                        (take 5))]

        (if (= result '(\0 \0 \0 \0 \0))
          counter
          (recur (inc counter)))))))

(defn day4-2
  []
  (let [input "bgvyzdsv"]
    (loop
      [counter 0]
      (let [
            result (->> counter
                        (str input)
                        m/md5
                        (take 6))]

        (if (= result '(\0 \0 \0 \0 \0 \0))
          counter
          (recur (inc counter)))))))

(day4-2)
