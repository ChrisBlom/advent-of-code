(ns adventofcode.2025.day09
  (:require
   [clojure.math :as m]
   [adventofcode.utils :as u]
   [clojure.string :as str]))

(def ex
  "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(defn parse-line [l]
  (read-string (format "[%s]" l)))

(defn parse [x]
  (mapv parse-line (str/split-lines x)))



(defn area [ [ya xa] [yb xb]]
  (let [ya* (min ya yb)
        yb* (max ya yb)
        xa* (min xa xb)
        xb* (max xa xb)

        dy (inc (- yb ya))
        dx (inc (- xb xa))]

    (* dy dx)))

(assert (= 6 (area [2 3] [7 3])))
(assert (= 24 (area [2 5] [9 7])))
