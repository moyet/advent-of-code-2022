(ns helpers.help
  (:require [clojure.string :as str]
            [clj-http.client :as hc]))

(def cookue
  {:session
   {
    :value
    "53616c7465645f5f36c1288907f43a311fa7e76833f19efa9fef9e9a5927efe3c4603bf0e2c997cec81da11f6988e068c66dd14ef660edbfb7161b19c518d28b"}})

(def date-url
  "https://adventofcode.com/%s/day/%s/input" )

(defn get-data-from-this-day
  [year day]
  (let [a-response (hc/get
                     (format date-url year day)
                     {:cookies cookue})]
  (:body a-response)))