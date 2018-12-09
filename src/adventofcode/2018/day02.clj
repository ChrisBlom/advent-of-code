(ns adventofcode.2018.day02
  (:require [midje.sweet :refer :all]
            [clojure.java.io :as io]))

(def example
  ["abcdef"
   "bababc"
   "abbcde"
   "abcccd"
   "aabcdd"
   "abcdee"
   "ababab"])

(defn counts [m]
  [(if (seq (m 2)) 1 0)
   (if (seq (m 3)) 1 0)])

(defn preproces [s]
  (-> s
      frequencies
      (->> (group-by val))
      counts))

(defn part-1 [inputs]
  (->> inputs
       (map preproces)
       (reduce (fn [[doubles triples] [d t]]
                 [(+ doubles d) (+ triples t)]))
       (apply *)))

(def input
  (->> "2018/day02.txt"
       io/resource
       io/reader
       line-seq
       vec))

(def example-2
  ["abcde"
   "fghij"
   "klmno"
   "pqrst"
   "fguij"
   "axcye"
   "wvxyz"])

(defn diff-count [a b]
  (-> (map = a b)
      frequencies
      (get false)))

(defn same-chars [a b]
  (apply str (mapcat #(if (= %1 %2) [%1] []) a b)))

{:part-1 (part-1 input)
 :part-2 (let [sorted-input (sort input)]
           (-> (map list sorted-input (rest sorted-input))
               (->> (group-by (fn [[a b]] (diff-count a b))))
               (get 1)
               first
               (->> (apply same-chars))))}
