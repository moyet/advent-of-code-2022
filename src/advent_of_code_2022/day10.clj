(ns advent-of-code-2022.day10
  (:require [clojure.string :as str]
            ))

(def signal (atom {}))
(def value (atom 1))
(def tick (atom 1))
(def screen (atom []))
(def values (atom []))


(defn day10-question1 []
  (let [
        input (->>
                "resources/day10.data"
                slurp
                str/split-lines
                (map #(str/split % #" "))
                )
        importants-ticks [20 60 100 140 180 220]
        ]

    (doseq [line input]

      (let [[operation change] line]
        (if (= operation "noop")
          (do
            (swap! tick inc)
            (swap! signal assoc @tick {:line line :value @value :signal (* @tick @value)})
            )
          (do
            (swap! tick inc)
            (swap! signal assoc @tick {:line line :value @value :signal (* @tick @value)})

            (swap! tick inc)
            (swap! value + (Integer/parseInt change))
            (swap! signal assoc @tick {:line line :value @value :signal (* @tick @value)})
            )
          )
        )
      )
    (apply + (map #(:signal (get @signal %)) importants-ticks))
    )
  )


(defn day10-question2 []
  (reset! screen [])
  (let [
        input (->>
                "resources/day10.data"
                slurp
                str/split-lines
                (map #(str/split % #" "))
                )
        ]
    (swap! signal assoc @tick {:value @value :signal (* @tick @value)})
    (swap! values conj (* @tick @value))

    (doseq [line input]

      (let [[operation change] line]
        (if (= operation "noop")
          (do
            (swap! tick inc)
            (swap! signal assoc @tick {:line line :value @value :signal (* @tick @value)})
            (swap! values conj (* @tick @value))
            )
          (do
            (swap! tick inc)
            (swap! signal assoc @tick {:line line :value @value :signal (* @tick @value)})
            (swap! values conj (* @tick @value))
            (swap! tick inc)
            (swap! value + (Integer/parseInt change))
            (swap! signal assoc @tick {:line line :value @value :signal (* @tick @value)})
            (swap! values conj (* @tick @value))
            )
          )
        )
      )
    )
  (for [i (range 1 242)]
    (let [val (get (@signal i) :value)
          pos (mod i 40)
          ]
      (if (contains? #{-1 0 1} (- (mod pos 40) val))
        (swap! screen conj \#)
        (swap! screen conj \.)
        )

      (let [
            parts (partition 40 @screen)
            sss (map str/join parts)
            output (str/join "\n" sss)
              ]
        (apply str output)
        )
      )
    )
  )


(defn perform [cycles [op param]] (let [x (peek cycles)] (
                                                           if (= op "noop")
                                                           (conj cycles x)
                                                           (into cycles [x (+ x (Integer/parseInt param))])
                                                           )))

(defn draw [[screen pos] sprite] (
                                   if (contains? #{-1 0 1} (- (mod pos 40) sprite))
                                   [(conj screen "#") (inc pos)]
                                   [(conj screen ".") (inc pos)]
                                   ))

(with-open [rdr (clojure.java.io/reader "resources/day10.data")] (let [
                                                            lines (->> rdr line-seq (map #(clojure.string/split % #" ")))
                                                            history (reduce perform [1] lines)
                                                            ] (->> history
                                                                   (map-indexed vector)
                                                                   (filter #(-> % first inc (- 20) (mod 40) zero?))
                                                                   (reduce (fn [total [ix x]] (-> ix inc (* x) (+ total))) 0)
                                                                   println)
                                                              (->> history
                                                                   (reduce draw [[] 0])
                                                                   first
                                                                   (partition 40)
                                                                   (map clojure.string/join)
                                                                   (clojure.string/join "\n")
                                                                   println)
                                                              ))
