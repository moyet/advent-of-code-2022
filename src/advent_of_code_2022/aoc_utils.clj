(ns advent-of-code-2022.aoc-utils
  (:require [hato.client :as hc]
            [clj-http.client :as client]
            )
  (:import
  [java.net CookieManager URI]
  [java.time Period ZonedDateTime]))

(def c (hc/build-http-client {:connect-timeout 10000
                              :redirect-policy :always
                              }))

(def session-cookie
  ["session" {:value "53616c7465645f5facc39a4bb9b80d0b7b142d248894ce898ab7ec672004deeff8d13214173cce30ceb22361211623c9960a5094d55d5761f8714ba55bf2788a"
              :domain "https://adventofcode.com"
              }]
  )

(def my-cookie(clj-http.cookies/to-basic-client-cookie session-cookie ))

(def cs
  (clj-http.cookies/cookie-store )
  )

(clj-http.cookies/add-cookie cs my-cookie)

(def input-url
  "https://adventofcode.com/2022/day/3/input"
  )


