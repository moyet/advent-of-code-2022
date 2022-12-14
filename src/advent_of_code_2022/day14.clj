(ns advent-of-code-2022.day14
  (:require [clojure.string :as str]
            [clojure.set :as set]
            ))

(def cave (atom []))
(def min-x (atom 0))
(def max-x (atom 0))
(def min-y (atom 0))
(def max-y (atom 0))
(def size (atom []))
(def abyss (atom false))
(def origin [500 0])

(defn to-int
  [pair]
  (map #(Integer/parseInt %) pair)
  )

(defn split-on->
  [line]
  (let [line (->
               line
               (str/split #"\s+->\s+"))
        line (->>
               line
               (map #(str/split % #","))
               (map to-int))
        ]
    line
    ))

(defn print-cave
  []
  (let [output (atom [])
        k (keys @cave)
        xs (sort (set (map first k)))
        ys (sort (set (map second k)))
        interesting-xs [(first xs) 500 (last xs)]
        ]

    (dotimes [n 3]
      (swap! output assoc n
             (apply str (map #(-> %
                                  str
                                  (nth n)) xs)
                    ))
      )
  (swap! output assoc 3 (apply str (repeat (count xs) \- )))

    (doseq [y ys]
      (swap! output assoc (+ y 4) (str (apply str (map #(get @cave [% y]) xs) ) "  " y) )

      )
    (println (str/join "\n" (map str/join @output)))
    (println (count (filter #(= % \O) (vals @cave))))
    )
  )

(defn draw-lines
  [from to]
  (let [same-x? (= (first from) (first to))
        minx (min (first from) (first to))
        maxx (max (first from) (first to))
        miny (min (second from) (second to))
        maxy (max (second from) (second to))
        ]

    (if same-x?
      (do
        (doseq [y (range miny (inc maxy))]
          (swap! cave assoc [maxx y] \#)
          )
        )
      (do
        (doseq [x (range minx (inc maxx))]
          (swap! cave assoc [x maxy] \#)
          )
        )
      )
    )
  )

(defn handle-input
  [line]
  (let [from (first line)
        to (second line)
        ]
    (if (not (nil? to))
      (do
        (draw-lines from to)
        (handle-input (rest line))
        ))
    )
  )

(defn drop-sand
  [point]
  (let [[x y] point
        abyss? (or (< x @min-x)
                   (> x @max-x)
                   (< y @min-y)
                   (> y @max-y))
        below? (or
                 (= \. (get @cave [x (inc y)]))
                 (nil? (get @cave [x (inc y)]))
                 )
        below-left? (or
                      (= \. (get @cave [(dec x) (inc y)]))
                      (nil? (get @cave [(dec x) (inc y)]))
                      )
        below-right? (or
                       (= \. (get @cave [(inc x) (inc y)]))
                       (nil? (get @cave [(inc x) (inc y)]))
                       )
        ]

    (if abyss?
      (do
        (println "We have reached the abyss")
        (reset! abyss true)
        )
      (if below?
        (do
          (drop-sand [x (inc y)])
          )

        (if below-left?
          (do
            (drop-sand [(dec x) (inc y)])
            )
          (if below-right?

            (do
              (drop-sand [(inc x) (inc y)])
              )
            (do
              (println "I cannot drop anymore" point)
              (swap! cave assoc point \O)
              ))
          )
        )
      )
    ))


(defn drop-sand-again
  [point]
  (let [[x y] point]

    (if (or (<= x @min-x)
            (>= x (dec @max-x))
            )
      (do
        (println "We have reached an edge. Time to resize everything")
        (println "Point " point)
        (println "Min Max x " @min-x @max-x)

        (swap! min-x dec)
        (swap! max-x inc)

        (dotimes [y (inc @max-y)]
          (swap! cave assoc [(dec @max-x) y] \.)
          (swap! cave assoc [@min-x y] \.))

        (swap! cave assoc [@max-x (inc @max-y)] \#)
        (swap! cave assoc [@min-x (inc @max-y)] \#)
        (drop-sand-again point)
        )
      )

      (let [
            below? (= \. (get @cave [x (inc y)]))
            below-left? (= \. (get @cave [(dec x) (inc y)]))
            below-right? (= \. (get @cave [(inc x) (inc y)]))]
        (if below?
          (do
            (drop-sand-again [x (inc y)])
            )

          (if below-left?
            (do
              (drop-sand-again [(dec x) (inc y)])
              )
            (if below-right?
              (do
                (drop-sand-again [(inc x) (inc y)])
                )
              (do
                (swap! cave assoc point \O)
                #_(print-cave)
                )
              )
            )
          )
        )


    )
  )

(defn as []
  (let [turns (atom -1)]
    (while (not @abyss)
      (do
        (swap! turns inc)
        (println "Turn no. " @turns)
        (drop-sand origin)
        (print-cave)
        ))
    (println "Sand has dropped to the abyss at turn " @turns)
    ))


(defn fill-up []
  (let [turns (atom -1)]
    (while (= \+ (get @cave origin) )
      (do
        (swap! turns inc)
        (println "Turn no. " @turns)
        (drop-sand-again origin)
        #_(print-cave)
        ))
    (println "Sand has dropped to the abyss at turn " @turns)
    ))


(defn question1
  []
  (reset! cave {})
  (let [input (->> "resources/day14.data"
                   slurp
                   str/split-lines
                   (map split-on->))
        xs (seq (flatten (map #(map first %) input)))
        ys (seq (flatten (map #(map second %) input)))
        ]
    (reset! min-x (apply min xs))
    (reset! max-x (inc (apply max xs)))
    (reset! min-y 0)
    (reset! max-y (inc (apply max ys)))
    (reset! size [(- @max-x @min-x) (- @max-y @min-y)])

    (doseq [x (range @min-x @max-x)]
      (doseq [y (range 0 @max-y)]
        (swap! cave assoc [x y] \.)
        )
      )

    (swap! cave assoc origin \+)

    (doseq [line input]
      (handle-input line)
      )
    )
  (as)
  )

(defn question2
  []
  (reset! cave {})
  (let [input (->> "resources/day14.data"
                   slurp
                   str/split-lines
                   (map split-on->))
        xs (seq (flatten (map #(map first %) input)))
        ys (seq (flatten (map #(map second %) input)))
        ]
    (reset! min-x (apply min xs))
    (reset! max-x (inc (apply max xs)))
    (reset! min-y 0)
    (reset! max-y (inc (apply max ys)))
    (reset! size [(- @max-x @min-x) (- @max-y @min-y)])

    (doseq [x (range @min-x @max-x)]
      (doseq [y (range 0 (inc @max-y))]
        (swap! cave assoc [x y] \.)
        )
      (swap! cave assoc [x (inc @max-y)] \#)
      )
    (swap! cave assoc origin \+)

    (doseq [line input]
      (handle-input line)
      )
    )

  )