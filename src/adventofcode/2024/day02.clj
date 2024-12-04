(ns adventofcode.2024.day02
  (:require [clojure.math :as math]))

(def ex "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn parse [x]
  (mapv #(read-string (format "[%s]" %) )
       (clojure.string/split-lines x)))

(defn safe? [nums]
  (let [diffs (map - (rest nums) nums)]
    (and (or (every? pos? diffs)
             (every? neg? diffs))
         (every? #(<= 1 (abs %) 3) diffs))))

(defn part-1 [x]
  (let [xs (parse x)]
    (count (filter safe? xs))))

(part-1 ex)

(defn without [index coll]
  (concat (take index coll) (drop (inc index) coll)))

(defn safe-2? [nums]
  (some safe?
        (cons nums
              (map-indexed (fn [i _] (without i nums)) nums))))

(defn part-2 [x]
  (count (filter safe-2? (parse x))))

(part-2 ex)

{:part-1 (part-1 (user/day-input))
 :part-2 (part-2 (user/day-input))}
