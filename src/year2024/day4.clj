(ns year2024.day4
  (:require [helpers.help :as help]
            [clojure.walk :as w]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math]
            ))

(defonce input (->> (help/get-data-from-this-day 2024 4)
                    str/split-lines
                    ))
(defn- as-map [matrix]
  (into {}
        (for [row (range (count matrix))
              col (range (count (first matrix)))
              :let [coord [row col]]]
          [coord (get-in matrix coord)])))


(def test-input (->>
                  "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
                  str/split-lines))


(defn coloums
  [input i]
  (->>
    input
    (map (fn [line]
           (nth line i)
           ))
    ))

(defn check-for-xmas
  [amap [x y]]
  (let [lodret-ned (and (= \M (get amap [x (+ 1 y)]))
                        (= \A (get amap [x (+ 2 y)]))
                        (= \S (get amap [x (+ 3 y)])))
        lodret-op (and (= \M (get amap [x (- y 1)]))
                       (= \A (get amap [x (- y 2)]))
                       (= \S (get amap [x (- y 3)])))
        vandret-h (and (= \M (get amap [(+ x 1) y]))
                       (= \A (get amap [(+ x 2) y]))
                       (= \S (get amap [(+ x 3) y])))
        vandret-l (and (= \M (get amap [(- x 1) y]))
                       (= \A (get amap [(- x 2) y]))
                       (= \S (get amap [(- x 3) y])))
        op-l (and (= \M (get amap [(- x 1) (- y 1)]))
                  (= \A (get amap [(- x 2) (- y 2)]))
                  (= \S (get amap [(- x 3) (- y 3)])))
        op-h (and (= \M (get amap [(- x 1) (+ y 1)]))
                  (= \A (get amap [(- x 2) (+ y 2)]))
                  (= \S (get amap [(- x 3) (+ y 3)])))
        ned-h (and (= \M (get amap [(+ x 1) (+ y 1)]))
                   (= \A (get amap [(+ x 2) (+ y 2)]))
                   (= \S (get amap [(+ x 3) (+ y 3)])))
        ned-l (and (= \M (get amap [(+ x 1) (- y 1)]))
                   (= \A (get amap [(+ x 2) (- y 2)]))
                   (= \S (get amap [(+ x 3) (- y 3)])))]
    (-> [lodret-op lodret-ned vandret-l vandret-h op-h op-l ned-h ned-l]
        frequencies
        (get true))))

(defn p1
  [input]
  (let [amap (as-map input)
        x (count input)
        y (count (first input))
        r (for [i (range x) j (range y) :when (= (get amap [i j]) \X)]
            (check-for-xmas amap [i j]))]
    (->> r
         (filter integer?)
         (apply +))))


(defn check-for-mas
  [amap [x y]]
  (let [h-v (and (= \M (get amap [(- x 1) (- y 1)]))
                 (= \S (get amap [(+ x 1) (+ y 1)])))
        v-h (and (= \M (get amap [(+ x 1) (- y 1)]))
                 (= \S (get amap [(- x 1) (+ y 1)])))
        h-h (and (= \M (get amap [(+ x 1) (+ y 1)]))
                 (= \S (get amap [(- x 1) (- y 1)])))
        v-v (and (= \M (get amap [(- x 1) (+ y 1)]))
                 (= \S (get amap [(+ x 1) (- y 1)])))
        r (-> [h-h h-v v-v v-h]
              frequencies
              (get true))]
    r
    ))

(defn p2
  [input]
  (let [amap (as-map input)
        x (count input)
        y (count (first input))
        r (for [i (range x) j (range y) :when (= (get amap [i j]) \A)]
            (check-for-mas amap [i j]))]
    (->> r (filter integer?)
        (map (fn [n]
               (-> n
                   (/ 2)
                   math/floor)))
         (apply +)
         )
    )
  )

