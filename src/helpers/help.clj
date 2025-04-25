(ns helpers.help
  (:require [clojure.string :as str]
            [clj-http.client :as hc]))

(def cookue
  {:session
   {
    :value
    "53616c7465645f5ffb7e56cbe627bd08b70e1c39392c4d66b4e68c82439a1fa6dafe1ef062da1e1cbda01570c8b1c4c225c06ed04a06a1d40a94f7ceafbcf4ff"}})

(def date-url
  "https://adventofcode.com/%s/day/%s/input" )

(defn get-data-from-this-day
  [year day]
  (let [a-response (hc/get
                     (format date-url year day)
                     {:cookies cookue})]
  (:body a-response)))