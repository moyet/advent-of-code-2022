(ns year2024.day2
  (:require [helpers.help :as help]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math]
            ))


(defonce input
         (->> (help/get-data-from-this-day 2024 2)
              str/split-lines
              (map (fn [l]
                     (map #(Integer/parseInt %) (str/split l #"\s+"))))
              ))

(def test-input
  (map (fn [l]
         (map #(Integer/parseInt %) (str/split l #"\s+")))
  (str/split-lines "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9")
  ))

(defn safe?
  [l]
  (let [f (first l)
        r (rest l)]
    (cond
      (empty? r) true
      (and (< 0 (- (first r) f)) (< (- (first r) f) 4)) (recur r)
      :else false)))

(defn reallysafe?
  [l]
  (or (safe? l)
      (safe? (reverse l))))

(defn q1 [input]
  (let [pa (->> input
                (filter (comp not reallysafe?))
                )]
    pa))

(defn safe-again?
      [l error?]
      (let [f (first l)
            r (rest l)]
        (println l)
        (cond
          (empty? r) true
          (and (<= 1 (- (first r) f)) (<= (- (first r) f) 3))
          (do
            (recur r error?))
          error? (do
                   (println "error")
                   (recur r false))
          :else false)))

(defn reallysafe2? [l]
  (let [reverse? (> (first l) (second l))]
    (cond
      (= (first l) (second l)) (safe-again? (rest l) false)
      reverse? (safe-again? (reverse l) true)
      :else (safe-again? l true))))

(defn r2 [l]
  (or (safe-again? l true)
      (safe-again? (reverse l) true)
      )
  )

(defn q2 [input]
  (let [pa (->> input
                (filter (comp reallysafe2?))
                count
                )]
    pa))