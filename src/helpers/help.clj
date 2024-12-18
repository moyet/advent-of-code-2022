(ns helpers.help
  (:require [clojure.string :as str]
            [clj-http.client :as hc]))

(def cookue
  {:session
   {
    :value
    "53616c7465645f5f868a27282a37a594cc3f5232d12368eeb1d6c56bac21fd0fff95249a6de29eecda8a33e897b1005885c3a9cf06605eebdda3a73d2e7488e0"}})

(def date-url
  "https://adventofcode.com/%s/day/%s/input" )

(defn get-data-from-this-day
  [year day]
  (let [a-response (hc/get
                     (format date-url year day)
                     {:cookies cookue})]
  (:body a-response)))