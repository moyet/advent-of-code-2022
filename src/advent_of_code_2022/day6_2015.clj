(ns advent-of-code-2022.day6-2015
  (:require [clojure.string :as str]))

(def actions
  {:on (constantly true)
   :off (constantly false)
   :toggle not
   })

(def day2-actions
  {:on inc
   :off #(max 0 (dec %))
   :toggle (partial + 2)
   })


(defn init-grid
  "Returns a lazy n-by-n grid
  where the value of every element is the given constant."
  [n constant]
  (repeat n (repeat n constant)))

(defn form-grid
  [n state-fn]
  (let [grid-size (range n)]
    (persistent!
      (reduce
        (fn [row x]
          (conj! row (persistent!
                       (reduce
                         (fn [column y]
                           (conj! column (state-fn [x y])))
                         (transient []) grid-size))))
        (transient []) grid-size))))

(defn light-a-row
  [op from to lights]
  (let [lenght    (- to from )
        start-vec (take from lights)
        midt-vec  (case op
                    :on (take lenght (repeat true))
                    :off (take lenght (repeat false))
                    :toggle (->> lights
                                 (drop from)
                                 (take lenght)
                                 (map not)))
        slut-vec  (take-last to lights)]
    (concat start-vec midt-vec slut-vec)))

(defn- light-it-all
  [lights op to from]
  (let [[tx ty] to
        [fx fy] from
        start-vec (take fx lights)
        midt-vec  (->> lights
                       (drop fx)
                       (take (- tx fx))
                       (map #(light-a-row op fy ty %))
                       )
        slut-vec  (take-last tx lights)]
    (concat start-vec midt-vec slut-vec)
    ))

(defn- do-something
  [lls]
  (loop
    [ss {:lines lls
         :lights (repeat 1000 (repeat 1000 false))}]
    (println "lights " (count (:lights ss)))
    (let [operations (:lines ss)
          lights (:lights ss)
          operation (first operations)]
      (if (nil? operation) (first lights)
                           (let []
                             (recur {:lines (rest operations)
                                     :lights lights}))))))

(defn transform-grid
  "Applies an instruction to a grid and returns the modified grid."
  [actions grid instruction]
  (let [action ((first instruction) actions)
        [xr yr] (rest instruction)]
    (form-grid
      (count grid)
      (fn [[x y]]
        (let [light (nth (nth grid x) y)]
          (if (and (<= (first xr) x (last xr))
                   (<= (first yr) y (last yr)))
            (action light) light))))))

(defn parse-instruction
  "Converts an instruction from a string to a vector,
  e.g., turn on 0,5 through 2,10 becomes [:on [0 2] [5 10]]."
  [string]
  (let [matches
        (rest (re-matches
                #"^(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)$"
                string))
        command (keyword (clojure.string/replace (first matches) "turn " ""))
        lights (map #(Integer. %) (rest matches))]
    [command (take-nth 2 lights) (take-nth 2 (rest lights))]))
(defn day6-1
  []
  (let [input
        (->> "resources/2015-day6.data"
             slurp
             str/split-lines
             (map parse-instruction))
        start-matrix (init-grid 1000 false)]
    (reduce (partial transform-grid actions)
            start-matrix input)))


(defn day6-2
  []
  (let [input
        (->> "resources/2015-day6.data"
             slurp
             str/split-lines
             (map parse-instruction))
        start-matrix (init-grid 1000 0)]
    (reduce + (flatten (reduce (partial transform-grid day2-actions)
            start-matrix input)
    ))
    ))
