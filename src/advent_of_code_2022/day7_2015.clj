(ns advent-of-code-2022.day7-2015
  (:require [clojure.string :as str]
            [clj-http.client :as hc]
            ))


(def commands
  {:AND bit-and
   :OR  bit-or
   :NOT bit-not
   :NONE identity
   :RSHIFT bit-shift-right
   :LSHIFT bit-shift-left})

(defn string->int-or-keyword
  [input]
  (if (re-matches #"[a-z]+" input)
    (keyword input)
    (Integer/parseInt input)
    )
  )
(defn- parse-input-string
  [input-string]
  (let [match (re-matches #"([0-9a-z]+)? (AND|OR|RSHIFT|LSHIFT|NOT) ([a-z0-9]+)+ -> (\w+)"  input-string)]
    (if match
      (let
        [[_ first-operand operant second-operand result ] match]
        {:op (keyword operant)
         :first (string->int-or-keyword first-operand)
         :second (string->int-or-keyword second-operand)
         :result (keyword result)})
      (let [[_ opererant first-operand result] (re-matches #"(NOT) (\w+) -> (\w+)" input-string)]
        (if opererant
         {:op (keyword opererant)
         :first (string->int-or-keyword first-operand)
         :result (keyword result)
         }
         (let [match (re-matches #"([0-9a-z]+) -> (\w+)" input-string)]
           (if match
             {:op :NONE
              :first (string->int-or-keyword (second match))
              :result (keyword (last match))})))))))

(defn do-something [results {:keys [op first second result]}]
  (let [two-sixteen (int (Math/pow 2 16))
        commando (op commands)
        f (if (int? first)
            first
            (get results first 0)
            )
        s (if (int? second)
            second
            (get results second 0)
            )
        new-result (case op
                     :NOT (commando f)
                     :NONE (commando f)
                     (commando f s))
        ]
      (assoc results result  new-result  )))


(defn day7-1
  []
  (let [input
          (->> "resources/2015-day7.data"
               slurp
               str/split-lines
               (map parse-input-string)
               )]
    (reduce do-something {} input)
    )
  )
