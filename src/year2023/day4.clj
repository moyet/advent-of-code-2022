(ns year2023.day4
  (:require [helpers.help :as help]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math]
            ))

(def cards (atom []))

(defonce input
         (->> (helpers.help/get-data-from-this-day 2023 4)
              str/split-lines
             ))

(def test-input
  (->>
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    str/split-lines
    )
  )

(defn score-a-card
  [{:keys [card-number numbers winners]}]

  (let [intersec (set/intersection numbers winners)
        counted   (count intersec)]
    (if (pos? counted)
      (math/expt 2 (dec counted))
      0)))

(defn update-cards
  [tickets card-number]
  (swap! cards
         update-in
         [card-number :tickets]
         #(+ % tickets)))
(defn score-a-card-again
  [cnumber]
  (let [card (get @cards cnumber)
        {:keys [card-number numbers winners tickets]} card
        intersec (set/intersection numbers winners)
        counted (count intersec)
        ]
    (when (pos? counted)
      (let [my-range (range (inc card-number) (+ card-number counted 1))]
        (map (partial update-cards tickets)  my-range )))))

(defn read-a-card
  [s]
  (let [[_ card-number numbers winners] (re-matches #"Card\s+(\d+):\s(.*)\|\s(.*).*" s)
        card-number (-> card-number
                        (#(Integer/parseInt %)))
        numbers (->> numbers
                     (re-seq #"\b(\d+)\b")
                     (map first)
                     (map #(Integer/parseInt %))
                  )
        winners (->> winners
                     (re-seq #"\b(\d+)\b")
                     (map first)
                     (map #(Integer/parseInt %)))]
    {:card-number card-number
       :numbers     (set numbers)
       :winners     (set winners)
     :tickets 1}))

(defn q1
    [i]
    (let [cards (->> i
                     (map read-a-card)
                     (map score-a-card)
                     (apply +)
                     )
          ]
      (println cards)))

(defn q2
  [i]
  (let [c (->> i
               (map read-a-card)
               (map (juxt :card-number identity) )
               (into (sorted-map))
               )
        _ (reset! cards c)
        card-numbers (keys c)]
    (for [card card-numbers]
      (score-a-card-again card)
      )
    )
  )