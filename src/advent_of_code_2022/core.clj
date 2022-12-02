(ns advent-of-code-2022.core
  (:require [clojure.string :as str])
  )

(defn sum-it
  [x]
  (->>
    x
    (map #(Integer/parseInt %))
    (apply +)
    )
  )

(defn day1
  []
  (let [
        input (->>
                "resources/day1.data"
                slurp
                str/split-lines
                (partition-by #(= "" %))
                (filter #(not= '("") %))
                (map sum-it))
        question1 (apply max input)
        question2 (->> input
                       (sort-by -)
                       (take 3)
                       (apply +)
                       )]
    [question1 question2]))


(defn- calculate-score
  [s]
  (let [
        dubbel (str/split s #" ")
        opponent (first dubbel)
        me (last dubbel)
        score (case me
                "X" 1
                "Y" 2
                "Z" 3)]
    (if
      (or
        (and (= opponent "A") (= me "X"))
        (and (= opponent "B") (= me "Y"))
        (and (= opponent "C") (= me "Z")))
      (+ score 3)
      (if
        (or
          (and (= opponent "A") (= me "Y"))
          (and (= opponent "B") (= me "Z"))
          (and (= opponent "C") (= me "X")))
        (+ score 6)
        score))))


(defn calculate-score-2
  [s]
  (let [rpc {:rock     1
             :paper    2
             :scissors 3}
        xyz {"X" 0
             "Y" 3
             "Z" 6}
        dubbel (str/split s #" ")
        opponent (first dubbel)
        me (last dubbel)

        score (xyz me)

        mc (case opponent
             "A" (case me
                   "X" :scissors
                   "Y" :rock
                   "Z" :paper
                   )
             "B" (case me
                   "X" :rock
                   "Y" :paper
                   "Z" :scissors)
             "C" (case me
                   "X" :paper
                   "Y" :scissors
                   "Z" :rock))]
    (+ score (rpc mc))))

(defn day2
  []
  (let [question1 (->>
                    "resources/day2.data"
                    slurp
                    str/split-lines
                    (map calculate-score)
                    (apply +)
                    )
        question2 (->>
                    "resources/day2.data"
                    slurp
                    str/split-lines
                    (map calculate-score-2)
                    (apply +))]

    [question1 question2]))