(ns advent-of-code-2022.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            )
  )

(def file-system (atom {}))
(def local-dir (atom []))
(def new-atom (atom {}))

(defn cd [dir]
  (if (= ".." dir)
    (do
      (let [mm (conj (vec (map str @local-dir)) :size)
            sub-dir-size (get-in @new-atom mm)]
        (swap! local-dir drop-last)
        (println "Time to calculate size")
        (let [mm (conj (vec (map str @local-dir)) :size)
              old-size (get-in @new-atom mm)
              new-size (+ old-size sub-dir-size)]
          (swap! new-atom assoc-in mm new-size)
          )
        )
      )
    (do
      (swap! local-dir concat [dir])
      (swap! file-system assoc-in (map str @local-dir) {})
      (swap! new-atom assoc-in (map str @local-dir) {:size 0})
      ))
  )

(defn ls [])

(defn handle-line
  [input]
  (do
    (let [
          [_ command operand] (str/split input #"(\s)")]
      (case command
        "cd" (cd (str operand))
        "ls" (ls)
        )
      )))

(defn handle-result
  [input]
  (let
    [
     [size file-name] (str/split input #"(\s)")
     ]
    (if (= size "dir")
      ;; Append a dir

      (do
        (swap! file-system assoc-in (concat (map str @local-dir) [file-name]) {})
        )                                                   ;; Append a file
      (do
        (let [
              file-size (-> size
                            Integer/parseInt
                            )
              new-file-system (update-in
                                @file-system
                                (map str @local-dir)
                                conj {(str file-name) file-size}
                                )
              ]
          (reset! file-system new-file-system)
          )
        )
      )
    )
  )

(defn handle-input
  [input]
  (if (= \$ (first input))
    ;;Command
    (do
      (let [
            [_ command operand] (str/split input #"(\s)")]
        (println "operand " operand)
        (case command
          "cd" (cd (str operand))
          "ls" (ls)
          )
        )
      )

    ;; ls result
    (do
      (let
        [
         [size file-name] (str/split input #"(\s)")
         ]
        (if (= size "dir")
          ;; Append a dir
          (do
            (println "Im a dir")
            )

          ;; Append a file
          (do
            (let [
                  file-size (-> size
                                Integer/parseInt
                                )
                  mm (conj (vec (map str @local-dir)) :size)
                  old-size (get-in @new-atom mm)
                  new-size (+ old-size file-size)
                  ]

              (swap! new-atom assoc-in mm new-size)
              )
            )
          )
        )
      )
    )
  )

(defn keys-in
  "Returns a sequence of all key paths in a given map using DFS walk."
  [m]
  (letfn [(children [node]
            (let [v (get-in m node)]
              (if (map? v)
                (map (fn [x] (conj node x)) (keys v))
                [])))
          (branch? [node] (-> (children node) seq boolean))]
    (->> (keys m)
         (map vector)
         (mapcat #(tree-seq branch? children %)))))

(defn start-over
  [n]
  (if (number? n)
    (do
      (println "We have a number" n)
      n)
    (let [
          values (atom [])
          ]
      (doseq [s n]
        (let [
              [key value] s
              sums (start-over value)

              ]
          (println (type sums))

          )
        )

      )
    )

  )

(defn day7
  []
  (let [inputs (->>
                 "resources/day7.data"
                 slurp
                 (str/split-lines)
                 )
        ]

    (for [input inputs]
      (handle-input input)
      )
    )
  )

(defn day77
  []
  (let [
        inputs (->>
                 "resources/day7.data"
                 slurp
                 (str/split-lines)
                 )
        ]
    (doseq [input inputs]
      (handle-input input)
      )

    (let [size-places (filter #(= :size (last %)) (keys-in @new-atom))
          sizes (map #(get-in @new-atom %) size-places)
          small-sizes (filter #(< % 100000) sizes)
          question1 (reduce + small-sizes)

          total-size (get-in @new-atom ["/" :size])
          free-space (- 70000000 total-size)
          needed-space (- 30000000 free-space)
          neeed-sizes (filter #(> % needed-space) sizes)

          ]
      (apply min neeed-sizes)
      )

    )
  )