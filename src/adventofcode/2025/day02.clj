(ns adventofcode.2025.day02
  (:require [clojure.string :as str]))

(def ex (str/join "" (str/split-lines  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")))


(defn parse-range [x]
  (map parse-long (str/split x #"-")))

(parse-range "77531934-77616074")

(defn parse [x]
  (let [x (str/trim-newline x)
        ranges (str/split x #",")]
    (map parse-range ranges)))


(defn expand-range [[l h]]
  (range l (inc h)))

(defn invalid? [x]
  (re-matches
   #"(\d{1,})\1"
   (str x)))

(invalid? "1")
(invalid? "11")
(invalid? "111")
(invalid? "1010")

(defn part-1 [ex]
  (reduce + (filter invalid? (mapcat expand-range (parse ex)))))

(defn part-2 [ex]
  (reduce + (filter invalid-2? (mapcat expand-range (parse ex)))))

(defn invalid-2? [x]
  (re-matches
   #"(\d{1,})\1{1,}"
   (str x)))

{:part-1
 (part-1 (user/day-input))
 :part-2
 (part-2 (user/day-input))}
