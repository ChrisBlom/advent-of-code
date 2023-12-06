(ns adventofcode.2023.day04
  (:require [clojure.java.io :as io]))

(require
 '[clojure.string :as str]
 '[clojure.set :as set])

(def example
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn parse-line [line]
  (let [[_ id winning-nums have-nums] (str/split line #"(Card\ +)|:|\|")]
    [(parse-long id)
     {:id (parse-long id)
      :winning-nums (read-string (format "[%s]" winning-nums))
      :have-nums (read-string (format "[%s]" have-nums))}]))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map parse-line)
       (into (sorted-map))))

(defn score [{:keys [winning-nums have-nums]}]
  (if-some [overlap (seq (filter (set  winning-nums) have-nums))]
    (bit-shift-left 1 (dec (count overlap)))
    0))

(defn part-1 [input]
  (->> input
       parse-input
       vals
       (map score)
       (reduce + 0)))

(defn part-2 [input]
  (let [input (parse-input input)        ]
    (->>  input
          (reduce-kv (fn [{:keys [multipliers cards-won]} id {:keys [winning-nums have-nums]}]
                       (let [multiplier (get multipliers id)
                             matches (count (filter (set winning-nums) have-nums))
                             won-card-ids (range (inc id) (inc (+ id matches)))]
                         {:multipliers (merge-with + multipliers (zipmap won-card-ids (repeat multiplier)))
                          :cards-won (update cards-won id + multiplier)}))
                     {:multipliers (zipmap (keys input) (repeat 1))
                      :cards-won (zipmap (keys input) (repeat 0))})
          :cards-won
          vals
          (reduce + 0))))

(assert (= (part-2 example) 30))

(let [input (slurp (io/resource "2023/day04.txt"))]
  (doto
      {:part-1 (part-1 input)
       :part-2 (part-2 input)}
    println))
