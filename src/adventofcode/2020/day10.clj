(ns adventofcode.2020.day10
  (:require
   [clojure.java.io :as io]))

(def ex [16 10 15 5 1 11 7 19 6 12 4])

(def ex2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(def input (read-string (format "[%s]" (slurp (io/resource "2020/day10.txt")))))

(defn jolt-jumps [adapters]
  (let [adapters (sort adapters)]
    (map -
         adapters
         (cons 0 adapters))))

(defn differences [ex]
  (frequencies
   (cons 3 (jolt-jumps ex))))

(defn part-1 [adapters]
  (let [direction (differences adapters)]
    (* (direction 1) (direction 3))))

(def ways-to-arrange-sequence-of-ones
  {[1]
   1

   [1 1]
   (count #{[1 1] [2]})

   [1 1 1]
   (count
    #{[ 1 1 1]
      [ 1  2 ]
      [  2 1 ]
      [   3  ]})

   [ 1 1 1 1]
   (count
    #{[1 1 1 1]
      [1 1  2 ]
      [1  2  1]
      [ 2 1 1 ]
      [ 2   2 ]
      [  3   1]
      [1   3  ]})})

(defn possibilities [changes]
  (case (first changes)
    3 1
    1 (get ways-to-arrange-sequence-of-ones changes 1)))

{:part-1
 (part-1 input)
 :part-2
 (reduce * (map possibilities (partition-by identity (jolt-jumps input))))}
