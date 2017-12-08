(ns adventofcode.2017.day08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse [input]
  (->> input
       str/split-lines
       (mapv (comp read-string (partial format "[%s]")))))

(def example
  (parse
   "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"))

(def input
  (parse (slurp (io/resource "2017/day08.txt"))))

(def != not=)

(defn solutions [input]
  (-> (reduce
       (fn [[mem highest] [register modify-sym amount _ src compare-sym n]]
         (if ((eval compare-sym) (get mem src 0) n)
           (let [new-val ((case modify-sym inc + dec -) (get mem register 0) amount)]
             [(assoc mem register new-val)
              (max highest new-val)])
           [mem highest]))
       [{} 0]
       input)
      (update 0 #(apply max (vals %)))))

(solutions input)
