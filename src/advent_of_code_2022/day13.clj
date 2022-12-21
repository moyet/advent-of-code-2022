(ns advent-of-code-2022.day13
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.edn :as edn]
            )
  )


(defn to-edn [strings]
  (map edn/read-string strings)
  )

(defn is-in-right-order?
  [left right]
  (let [left-int? (int? left)
        right-int? (int? right)
        case-point [left-int? right-int?]
        ]
    (println left right)
    (if (= left right)
      true
      (if (nil? left)
        (do
          true
          )
        (if (nil? right)
          (do
            false)

          (case case-point
            [true true] (do
                          (< left right)
                          )
            [true false] (do
                           (is-in-right-order? [left] right)
                           )
            [false true] (do
                           (is-in-right-order? left [right])
                           )
            (do
              (if (= (first left) (first right))
                (do
                  (is-in-right-order? (rest left) (rest right))
                  )
                (is-in-right-order? (first left) (first right))
                )
              )
            )
          )
        ))
    ))

(defn is-in-tight-order?
  [input]
  (is-in-right-order? (first input) (second input))
  )


(defn day13
  []
  (let [input (->>
                "resources/day13.data"
                slurp
                str/split-lines
                (partition-by #(= "" %))
                (filter #(= 2 (count %)))
                (map to-edn)
                )
        reslt (map is-in-tight-order? input)
        zippy (zipmap (range 1 (inc (count input))) reslt)
        question1 (apply + (keys (filter #(val %) zippy)))
        ]
    question1
    )
  )


(defn question2
  []
  (let [
        input (->> "resources/day13.data"
                   slurp
                   str/split-lines
                   (filter #(not= "" %))
                   (map edn/read-string)
                   )
        input (conj input [[2]])
        input (conj input [[6]])
        result (sort is-in-right-order? input)
        low (.indexOf result [[2]])
        high (.indexOf result [[6]])
        ]
    (* (inc low) (inc high))
    )
  )