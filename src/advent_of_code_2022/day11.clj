(ns advent-of-code-2022.day11
  (:require [clojure.string :as str]
            ))

(def monkees (atom []))


(defn monkee-throws
  [monkee]
  (let [operations {"*" *
                    "+" +}
        value (:value monkee)
        id (:id monkee)
        items (:items monkee)
        ]
    (doseq [mapped-item items]
      (let
        [
        item (:value mapped-item)

         operation (operations (monkee :operation))
         _ (println "Monkey " id " takes an item " item " and looks at it")
         value (if (= "old" value)
                 item
                 (Integer/parseInt value)
                 )

         new-value (operation item value)
         _ (println "Worry level is " (monkee :operation) " by " value " to " new-value " .")
         new-value (long (/ new-value 3))
         _ (println "Monkey gets bored with item. Worry level is divided by 3 to " new-value)

         test-result (= 0 (mod new-value (:test monkee)))

         recieving-monkee (->
                            test-result
                            str
                            keyword
                            monkee
                            )
         history {:monkee      id
                  :operation   operation
                  :start-value item
                  :next-monkee recieving-monkee
                  }
         mapped-item (assoc mapped-item :value new-value)
         mapped-item (update mapped-item :history concat [history])

         ]

        (println "Monkee tests the item it's " test-result)
        (println "Monkee throws the item to " recieving-monkee)

        (swap! monkees update-in [recieving-monkee :items] concat [mapped-item])
        (swap! monkees update-in [id :inspected] inc)
        (swap! monkees update-in [id :items] rest)
        )
      )
    )
  )

(defn do-a-round
  []
  (dotimes [monkee (count @monkees)]
    (monkee-throws (nth @monkees monkee))
    )
  )

(defn do-a-lot-of-rounds [nummer]
  (dotimes [round nummer]
    (do-a-round)
    )
  )

(defn day11 []
  (defn create-item [item]
    {
     :value item
     :history []
     })
  (let [
        input (->
                "resources/day11.data"
                slurp
                (str/split #"\n\n")
                )]
    (doseq [monkee input]
      (let [data (str/split-lines monkee)
            monkee-number (->> data
                               first
                               (re-find #"Monkey (\d+):")
                               second
                               Integer/parseInt
                               )

            items (map #(Integer/parseInt %)
                       (map str/trim
                            (str/split (second (re-find #"Starting items: (.*)" (second data))) #",")))

            [_ operation value] (->>
                                  (nth data 2)
                                  (re-find #"new = old ([+*]) (.+)")
                                  )

            [_ test] (->>
                       (nth data 3)
                       (re-find #"Test: divisible by (\d+)")
                       )

            [_ if-true] (->>
                          (nth data 4)
                          (re-find #"If true: throw to monkey (\d+)")
                          )
            [_ if-false] (->>
                           (nth data 5)
                           (re-find #"If false: throw to monkey (\d+)")
                           )

            mapped-items (map create-item items)
            ]



        (swap! monkees conj {:items     mapped-items
                             :operation operation
                             :value     value
                             :test      (Integer/parseInt test)
                             :true      (Integer/parseInt if-true)
                             :false     (Integer/parseInt if-false)
                             :inspected 0
                             :id        monkee-number
                             }
               )))
    (do-a-lot-of-rounds 20)
    (let [
          inspected (sort > (map :inspected @monkees))
          first-number (first inspected)
          second-number (second inspected)
          ]
      (* first-number second-number)
      )


    )
  )