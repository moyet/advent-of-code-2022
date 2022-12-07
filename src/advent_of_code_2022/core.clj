(ns advent-of-code-2022.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
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
  [line]
  (let [
        [opponent me] (str/split line #" ")
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
        [opponent me] (str/split s #" ")

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
                    (reduce +))]

    [question1 question2]))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn- split-in-half
  [sack]
  (let [halfed (/ (count sack) 2)
        [comp-1 comp-2] (split-at halfed sack)]
    (set/intersection (set comp-1) (set comp-2))))

(defn- get-intersection
  [[fst sec third]]
  (set/intersection (set fst) (set sec) (set third)))

(defn day3
  [file-name]
  (let [day3-scores (zipmap
                      (concat (char-range \a \z) (char-range \A \Z))
                      (range 1 53))
        question1 (->>
                    file-name
                    slurp
                    str/split-lines
                    (map split-in-half)
                    (map #(get-in day3-scores %))
                    (reduce +))
        question2 (->>
                    file-name
                    slurp
                    str/split-lines
                    (partition 3)
                    (map get-intersection)
                    (map #(get-in day3-scores %))
                    (reduce +))]
    [question1 question2]))

(defn day4
  [file-name]
  (let [
        find-subsets (fn [ranges]
                       (let [range-1 (range (-> ranges
                                                first
                                                Integer/parseInt)
                                            (->> ranges
                                                 second
                                                 Integer/parseInt
                                                 inc))
                             range-2 (range (->
                                              ranges
                                              (nth 2)
                                              Integer/parseInt)
                                            (-> ranges
                                                last
                                                Integer/parseInt
                                                inc))

                             set-1 (set range-1)
                             set-2 (set range-2)]

                         (or (set/subset? set-1 set-2)
                             (set/subset? set-2 set-1))))

        find-overlaps (fn [ranges]
                        (let [range-1 (range (-> ranges
                                                 first
                                                 Integer/parseInt)
                                             (->> ranges
                                                  second
                                                  Integer/parseInt
                                                  inc))
                              range-2 (range (->
                                               ranges
                                               (nth 2)
                                               Integer/parseInt)
                                             (-> ranges
                                                 last
                                                 Integer/parseInt
                                                 inc))

                              set-1 (set range-1)
                              set-2 (set range-2)]
                          (set/intersection set-1 set-2)))
        question1 (->>
                    file-name
                    slurp
                    str/split-lines
                    (map #(str/split % #"[-,]"))
                    (filter find-subsets)
                    count)

        question2 (->>
                    file-name
                    slurp
                    str/split-lines
                    (map #(str/split % #"[-,]"))
                    (map find-overlaps)
                    (filter #(not= 0 (count %)))
                    count)]
    [question1 question2]))


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

    #_(map move-something moves)
    #_(map move-something-more moves)
    )

  (->> (range 1 10)
       (map #(get @stacks %))
       (map last)
       (apply str)
       )
  )

(defn main
  []
  (day5 "resources/day5.data")
  )