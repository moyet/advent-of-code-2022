(ns advent-of-code-2022.day12
  (:require [clojure.string :as str]
            [clojure.set :as set]
            ))

(def our-graph (atom []))
(def a-graph (atom {}))
(def visited (atom {}))


(def start (atom []))
(def end (atom []))
(def size (atom []))
(def solutions (atom []))


(defn get-height [node]
  (let [height (->
                 @our-graph
                 (nth (first node))
                 (nth (second node))
                 )]
    (if (= height \S)
      97
      (if (= height \E)
        122
        (int height)
        )
      )
    )
  )


(defn lazy-contains? [col key]
  (some #{key} col))


(defn add-nodes [n1 n2]
  [(+ (first n1) (first n2)) (+ (second n1) (second n2))]
  )

(defn get-neighbors [node]
  (println node (->
                  @our-graph
                  (nth (first node))
                  (nth (second node))
                  ))

  (filter #(and
             (not (neg? (first %)))
             (not (neg? (second %)))
             (< (first %) (first @size))
             (< (second %) (second @size))
             )
          (map #(add-nodes % node) [[1 0] [-1 0] [0 1] [0 -1]])))

(defn find-a-way [node road-to-node]
  (let [height (get-height node)
        neighbors (get-neighbors node)
        neighbors (filter #(contains? [-1 0 1] (- (get-height %) height)) neighbors)
        neighbors (filter #(not (lazy-contains? road-to-node %)) neighbors)]
    (if (empty? neighbors)
      (do
        []
        )

      (if (lazy-contains? neighbors @end)
        (do
          (println "done" (conj road-to-node @end))
          (swap! solutions conj (conj road-to-node @end))
          (conj road-to-node @end)
          )
        (doseq [neighbor neighbors]
          (find-a-way neighbor (conj road-to-node neighbor))
          )
        ))
    ))

(defn day12 []
  (let [input (->>
                "resources/day12.data"
                slurp
                str/split-lines)
        rows (count input)
        columns (count (first input))
        ]
    (reset! our-graph input)
    (reset! size [rows columns])
    (dotimes [x rows]
      (let [row (-> input
                    (nth x)
                    )
            S (.indexOf row "S")
            E (.indexOf row "E")
            ]
        (if (not= -1 S)
          (reset! start [x S])
          ) test
        (if (not= -1 E)
          (reset! end [x E])
          )
        )
      )
    )
  )

(defn stringed [node]
  (str (first node) ";" (second node))
  )

(defn de-stringed [node]
  (map #(Integer/parseInt %) (str/split node #";"))
  )

(defn visit-a-node
  []
  (let [[node distance] (->>
                          @a-graph
                          (sort-by second)
                          first
                          )
        node (de-stringed node)

        height (get-height node)
        neighbors (get-neighbors node)

        ;(filter #(contains? [0 1 -1] (- (get-height %) height)) neighbors)
        neighbors(filter #(<  (- (get-height %) height) 2) neighbors)

        neighbors (filter #(not (get @visited (stringed %))) neighbors)
        ]

    (println (map get-height neighbors))

    (swap! a-graph dissoc (stringed node))
    (swap! visited assoc (stringed node) distance)

    (doseq [neighbor neighbors]
      (swap! a-graph assoc (stringed neighbor) (inc distance ))
      )
    (swap! our-graph assoc-in node \.)
    )

  )


(defn d12
  []
  (reset! visited {})
  (let [input (->>
                "resources/day12.data"
                slurp
                str/split-lines)
        rows (count input)
        columns (count (first input))]
    (reset! our-graph (vec (map vec input)))
    (reset! size [rows columns])
    (dotimes [x rows]
      (let [row (-> input
                    (nth x)
                    )
            S (.indexOf row "S")
            E (.indexOf row "E")
            ]
        (if (not= -1 S)
          (reset! start [x S])
          ) test
        (if (not= -1 E)
          (reset! end [x E])
          )
        )
      (dotimes [y columns]
        (swap! a-graph assoc (str x ";" y) ##Inf)
        )
      )
    (swap! a-graph assoc (str (first @start) ";" (second @start)) 0)
    )
  )

(defn main-part-1 []
  (while (> (count (filter #(<  (val %) 10000) @a-graph)) 0)
    (println "Theres is " (count @a-graph) "nodes still waiting to be looked at")
    (println "KNown nodes on @a-graph " (count (filter #(pos-int? (val %)) @a-graph)) )
    (visit-a-node)
    )
  (get @visited (stringed @end))
)

(defn o [] (dotimes [_ 100]
             (
               visit-a-node)
             )
  (map #(apply str %) @our-graph)
  )