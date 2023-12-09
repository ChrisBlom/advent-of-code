(ns adventofcode.2023.day09
  (:require [clojure.string :as str]))

(def example "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defn parse [s]
  (map (fn [line] (read-string (str "[" line "]"))) (str/split-lines s)))

(defn difference [xs]
  (mapv  - (rest xs) xs))

(defn differences-non-zero [nums]
  (->> (vec nums)
       (iterate difference)
       (take-while (fn [xs] (not (every? zero? xs))))))

(defn next-number [nums]
  (->>
   (differences-non-zero nums)
   (map peek)
   (reduce + 0)))

(assert (= 68 (next-number [10 13 16 21 30 45])))
(assert (= 10 (next-number [-2 2 6])))
(assert (= 10 (next-number [-20 -10 0])))

(def prev-number (comp next-number reverse))

(assert (= 5  (prev-number [10 13 16 21 30 45])))

{:part-1 (reduce + (map next-number (parse (user/day-input))))
 :part-2 (reduce + (map prev-number (parse (user/day-input))))}
