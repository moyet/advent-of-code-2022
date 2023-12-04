(ns year2023.day2
  (:require [helpers.help :as help]
            [clojure.string :as str]))

(defonce input
  (-> (help/get-data-from-this-day 2023 2)
      str/split-lines))

(def test-input
  (-> "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
      str/split-lines
      )
  )
(defn plus-a-map
  [map1 map2]
  {
   :red (+ (get map1 :red 0) (get map2 :red 0))
   :green (+ (get map1 :green 0) (get map2 :green 0))
   :blue (+ (get map1 :blue 0) (get map2 :blue 0))})

(defn maxed
  [map1 map2]
  {
   :red (max (get map1 :red 0) (get map2 :red 0))
   :green (max (get map1 :green 0) (get map2 :green 0))
   :blue (max (get map1 :blue 0) (get map2 :blue 0))}
  )

(defn understand
  [pull]
  (let [colours (->> pull
                     (re-seq #"(\d+) (red|green|blue)")
                     (map (fn [[_ antal farve]]
                               [(keyword farve) (Integer/parseInt antal)]))
                     (into {}))
        ]
    colours))

(defn legal-pull?
  [pull]
    (and (<= (get pull :red 0) 12)
         (<= (get pull :green 0) 13)
         (<= (get pull :blue 0) 14)))
(defn read-and-understand [s]
  (let [game-number (->> s
                         (re-find #"Game (\d+):")
                         last
                         Integer/parseInt)
        pulls (-> s
                  (str/split #": ")
                  second
                  (str/split #"; "))
        pulls (map understand pulls)
        legals (map legal-pull? pulls)
        ]
    {:game game-number
     :legal? (reduce #(and % %2) legals)
     :pulls (reduce maxed pulls)
     }))
(defn question1
  []
  (let [result
        (->> input
             (map read-and-understand)
             (filter :legal?)
             (map :game)
             (reduce +)
             )]
    result))

(defn question2
  []
  (let [result
        (->> input
             (map read-and-understand)
             (map :pulls)
             (map (fn muliplication
                    [{:keys [red green blue]}]
                    (* red green blue)))
             (reduce +))]
    result))