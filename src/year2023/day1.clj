(ns year2023.day1
  (:require [clojure.string :as str]
            [helpers.help :as help])
  )

(defonce input (str/split-lines (help/get-data-from-this-day 2023 1)))

(def test-input "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen")
(defn parse-a-line
  [reg-ex ss]
  (let [resu (re-seq reg-ex ss)
        f (first resu)
        s (last resu)]
    (Integer/parseInt  (str f s))))

(defn question1
  []
  (->> input
         (map (partial parse-a-line #"\d"))
         (apply +)))

(def numbers-as-strings
  {:one 1
   :two 2
   :seven 7
   :three 3
   :four 4
   :five 5
   :six 6
   :nine 9
   :zero 0
   :eight 8}
  )

(defn translations
  [i]
  (cond (= 1 (count i)) (Integer/parseInt i)
        :else (-> i
                  keyword
                  numbers-as-strings)))
(defn parse-a-line-again
  [reg-ex ss]
  (let [resu (re-seq reg-ex ss)
        f (-> resu first last)
        s (-> resu last last)]
    (Integer/parseInt(str (translations f) (translations s)))))

(defn question2
  []
  (->> input
       (map (partial parse-a-line-again #"(?=(\d|one|two|three|five|four|six|nine|seven|eight|zero))"))
       (apply +)))

