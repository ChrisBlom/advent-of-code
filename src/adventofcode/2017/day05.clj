(ns adventofcode.2017.day05
  (:require [midje.sweet :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (slurp (io/resource "2017/day05.txt")))

(def example
  "0
3
0
1
-3")

(defn solution [part input]
  (let [^ints instructions (->> input
                                clojure.string/split-lines
                                (map (comp int read-string))
                                (into-array Integer/TYPE))
        n (count instructions)
        part-2 (= part :b)]
    (time
        (loop [pos 0
               steps 0]
          (if (or (< pos 0) (>= pos n))
            steps
            (let [offset (aget instructions pos)]
              (aset instructions pos (if (and part-2 (>= offset 3))
                                       (dec offset)
                                       (inc offset)))
              (recur (+ pos offset)
                     (inc steps))))))))

(fact
  (solution :a example) => 5)

{:part-1 (solution :a input)
 :part-2 (solution :b input)}
