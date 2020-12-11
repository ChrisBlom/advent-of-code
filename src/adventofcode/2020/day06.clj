(ns adventofcode.2020.day06
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def example
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(def input (slurp (io/resource "2020/day06.txt")))

{:part-1
 (-> input
     (str/replace #"[ \n]" " ")
     (str/split #"  ")
     (->> (map #(str/split % #" "))
          (map (partial map set))
          (map (partial reduce set/union))
          (map count)
          (reduce +)))
:part-2
 (-> input
     (str/replace #"[ \n]" " ")
     (str/split #"  ")
     (->> (map #(str/split % #" "))
          (map (partial map set))
          (map (partial reduce set/intersection))
          (map count)
          (reduce +)))}

;; {:part-1 6726, :part-2 3316}
