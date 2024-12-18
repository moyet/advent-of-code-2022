(ns year2023.day3
  (:require [helpers.help :as help]
            [clojure.string :as str]
            [clojure.set :as set]
            )
  )

(defonce input
         (-> (help/get-data-from-this-day 2023 3)
             str/split-lines))

(def test-input (->
                  "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
                  str/split-lines
                  ))

(defn re-seq-pos [pattern string]
  (let [m (re-matcher pattern string)]
    ((fn step []
       (when (. m find)
         (cons {:start (. m start) :end (. m end) :group (. m group)}
               (lazy-seq (step))))))))

(defn get-position
  [input-string line-number number]
  (println "N" number)
  (let [pattern (re-pattern (str "[^0-9]+(" number ")[^0-9]+"))
        line (nth input-string line-number)
        stringt (re-find pattern line)

        line-lenght (count line)
        lenght (count number)
        number-of-times (-> pattern
                            (re-seq line)
                            count)
        _       (when (> number-of-times 1)
                  (println "WHATCHOUT" number number-of-times)
                  )
        start_ (str/index-of line stringt)
        the-range (range (max 0 (dec start_)) (min (dec line-lenght) (inc (min line-lenght (+ lenght start_)))))]
     the-range))

(defn get-numbers
  [line]
  (let [numbers (->> line
                     ((juxt first (fn [e]
                                    (->> e
                                         second
                                         (re-seq-pos #"(\d+)" )
                                         )))))]
    numbers))

(def possible-neighbors #{\* \# \+ \/ \% \= \@ \$ \& \- })
(defn- get-line-substring
  [s r]
  (map #(nth s %) r)
  )
(defn find-neighbors
  [input-string line-number {:keys [start end group]}]

  (let [number-of-lines (count input-string)
        line-before (max 0 (dec line-number))
        line-after  (min (dec number-of-lines) (inc line-number))
        line-range  (range line-before (inc line-after))
        lines (map #(nth input-string %) line-range)

        neighbors (-> (map
                        #(get-line-substring % (range (max 0 (dec start)) (min 139 (inc end))))
                        lines)
                      flatten
                      set)]
    (when (> (count (set/intersection possible-neighbors neighbors)) 0)
      (Integer/parseInt group))))


(defn q1-again
  [input-string]
  (let [find-neighbors-2 (partial find-neighbors input-string)
        n (->> input-string
               (zipmap (range))
               (map get-numbers)
               (into (sorted-map)))]
    (->> n
         (map (fn [[line-number numbers]]
                (let [find-neighbors-3 (partial find-neighbors-2 line-number)]
              (map find-neighbors-3 numbers))))
         flatten
         (filter number?)
         (reduce +))))

(defn substring-with-numbers
  [im-a-string]

  )
(defn find-q2-neighbors
  [input-string [line-number stars]]
  (let [number-of-lines (count input-string)
        before-line (dec line-number)
        after-line (min number-of-lines (inc line-number))
        lines-range (range before-line (inc after-line))
        lines (map #(nth input-string %) lines-range)]
    (->> stars
         (take 1)
         (map (fn [{:keys [start end]}]
                (let [min-start (max 0 (dec start))
                      max-end   (min 139 (inc end))
                      neighbors (zipmap lines-range
                                        (map #(get-line-substring % (range min-start max-end)) lines))
                      neighbors-with-numbers (filter
                                               (fn [[line-numbers chars]]
                                                 (->> chars
                                                      second
                                                      str
                                                      (re-matches #"\d")
                                                      )
                                                 )
                                               neighbors)
                      ]
                  neighbors
                  ))))))
(defn q2
  [input-string]
  (let [stars (->> input-string
                   (map (fn [a-line]
                          (re-seq-pos #"\*" a-line)))
                   (zipmap (range))
                   (filter (fn [[_ stars]]
                                (-> stars nil? not)
                                ))
                   (into (sorted-map)))]

    (->> stars
         (take 1)
         (map (partial find-q2-neighbors input-string))
           )
    )
  )