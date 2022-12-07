(ns advent-of-code-2022.day5
  (:require [clojure.string :as str]
            [clojure.set :as set]
            )
  )

(defn transpose [m]
  (apply mapv vector m))

(def stacks (atom {}))

(defn push-pop
  [to from]
  (let [from-value (last (get @stacks from))
        new-from (-> @stacks
                     (get from)
                     reverse
                     rest
                     reverse
                     )
        new-to (-> @stacks
                   (get to)
                   (concat [from-value])
                   )
        ]
    (swap! stacks assoc to new-to)
    (swap! stacks assoc from new-from)

    )
  )

(defn move-something [move]
  (let [
        [_ antal from to] (re-matches #"move (\d+) from (\d+) to (\d+)" move)
        from (Integer/parseInt from)
        to (Integer/parseInt to)]
    (println antal from to)
    (dotimes [_ (Integer/parseInt antal)]
      (push-pop to from)
      )))

(defn move-something-more
  [move]
  (let [
        [_ antal from to] (re-matches #"move (\d+) from (\d+) to (\d+)" move)
        from (Integer/parseInt from)
        to (Integer/parseInt to)
        antal (Integer/parseInt antal)

        from-elements (take-last antal (get @stacks from))
        new-to (-> @stacks
                   (get to)
                   (concat from-elements)
                   )
        new-from (drop-last antal (get @stacks from))
        ]
    (swap! stacks assoc to new-to)
    (swap! stacks assoc from new-from)

    )
  )

(defn day5 [filename]
  (let [
        input (->>
                filename
                slurp
                str/split-lines
                )
        blank-line (.indexOf input "")

        starting-positions-debug {1 [\Z \N]
                                  2 [\M \C \D]
                                  3 [\P]
                                  }
        starting-positions-matrix (->>
                                    input
                                    (split-at blank-line)
                                    first
                                    (map (fn [positions]
                                           (->>
                                             positions
                                             (partition 4 4 [\s])
                                             (map second)
                                             )
                                           ))
                                    (map #(take 9 (concat % (repeat \space))))
                                    reverse
                                    transpose
                                    (map #(filter (fn [z] (not= z \space)) %))
                                    )

        my-keys (map #(->
                        %
                        first
                        str
                        Integer/parseInt
                        ) starting-positions-matrix
                     )
        my-vals (map rest starting-positions-matrix)

        starting-positions (zipmap my-keys my-vals)

        _ (reset! stacks starting-positions)

        moves (->
                input
                (subvec (inc blank-line))
                )
        ]
    (println "moves " moves)
    (dotimes
      [n (count moves)]

      (move-something-more (nth moves n))
      )

    )

  (->> (range 1 10)
       (map #(get @stacks %))
       (map last)
       (apply str)
       )
  )
