(ns helpers.help
  (:require [clojure.string :as str]
            [clj-http.client :as hc]))

(def cookue
  {:session
   {
    :value
    "53616c7465645f5fd4890bc29bd8973aa982e16b2e6b95b93b0972e9118d92335a08684b41061017f610e8a324d39902fff136b70638222f4622d93dbcaa8faf"}})

(def date-url
  "https://adventofcode.com/%s/day/%s/input" )

(defn get-data-from-this-day
  [year day]
  (let [a-response (hc/get
                     (format date-url year day)
                     {:cookies cookue})]
  (:body a-response)))