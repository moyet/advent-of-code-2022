(ns year2015.day6
  (:require [helpers.help :as help]
            [clojure.string :as str]))




(def input (help/get-data-from-this-day 2015 6))

(def size 1000)
(def empty-matrix (vec (repeat size (vec (repeat size false)))))


(defn position->ints [position]
  (let [pos (str/split position #",")]
    (map #(Integer/parseInt %) pos)))

(defn set-submatrix [m row-start row-end col-start col-end funk]
  (reduce
    (fn [acc row]
      (update acc row
              (fn [r]
                (reduce
                  (fn [racc col]
                    (update racc col funk))
                  r
                  (range col-start (inc col-end))))))
    m
    (range row-start (inc row-end))))


(defn do-stuff [line matrix]
  (let [[_ command start-position end-position] (re-matches #"(turn on|turn off|toggle) (\d+,\d+) through (\d+,\d+)" line)
        sp (position->ints start-position)
        ep (position->ints end-position)
        x-range  (range (first sp) (-> ep
                                       first
                                       inc))
        y-range (range (last sp) (-> ep
                                     last
                                     inc))]


    (comment
      (update-in matrix [1] #(update-in % [2] not)))



    (case command
      "turn on" (do
                  (println "Turn on stuff")
                  (set-submatrix matrix
                                 (first ep)
                                 (-> ep last inc)
                                 (first sp)
                                 (-> sp last inc)
                                 (constantly true)))


      "turn off" (do
        (println "Turning off stuff")
        (set-submatrix matrix
                       (first ep)
                       (-> ep last inc)
                       (first sp)
                       (-> sp last inc)
                       (constantly false)))

      "toggle"    (do
                       (println "Toggling. ")
                       (set-submatrix matrix
                                      (first ep)
                                      (-> ep last inc)
                                      (first sp)
                                      (-> sp last inc)
                                      not)))))


(defn question-1
  []
  (let [pi (-> input
                str/split-lines
                first
                (do-stuff empty-matrix )
                )]


    pi))
