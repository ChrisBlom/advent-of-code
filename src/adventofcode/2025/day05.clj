(ns adventofcode.2025.day05
  (:require [clojure.string :as str]))

(def ex "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(defn parse-range [x]
  (mapv parse-long (str/split x #"-")))

(defn parse [x]
  (let [ [range [_ & ingredients]] (split-with (complement #{""}) (str/split-lines x))]
    {:ranges (sort-by first (mapv parse-range range))
     :ingredients (mapv parse-long ingredients)}))

(defn expand-range [[l h]]
  (range l (inc h)))

(defn part-1 [x]
  (let [{:keys [ranges ingredients]} (parse x)
        fresh? (fn [i] (some (fn [ [l r]] (<= l i r) ) ranges ))]
    (count (filter fresh? ingredients))))

(defn overlap? [ [a b] [c d]]
  (or (<= c a d)
      (<= c b d)
      (<= a c b)
      (<= a d b)))

(defn merge-interval [l r]
  (assert (overlap? l r))
  [(apply min (concat l r))
   (apply max (concat l r))])

(defn range-size [[l r]]
  (inc (- r l)))

(defn p2 [ [a b & tail]]
  (cond (not b)
        (range-size a)

        (overlap? a b)
        (recur (cons (merge-interval a b) tail))

        :else
        (+ (range-size a)
           (p2 (cons b tail)))))

(defn part-2 [x]
  (let [{:keys [ranges ingredients]} (parse x)]
    (p2 ranges)))

(time
 {:part-1 (part-1 (user/day-input))
  :part-2 (part-2 (user/day-input))})
