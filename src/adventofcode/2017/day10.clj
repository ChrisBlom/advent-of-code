(ns adventofcode.2017.day10
  (:require
   [clojure.string :as str]
   [midje.sweet :refer :all]))


(def input [183,0,31,146,254,240,223,150,2,206,161,1,255,232,199,88])

;;;; part 1

(def example [3, 4, 1, 5])

(defn hash-transition [ [v pos skip-size] length]
  (let [n (count v)
        to-swap (map #(mod % n) (range pos (+ pos length)))
        mapping (map list to-swap (reverse to-swap))]
    [(reduce (fn [acc [from to]] (assoc acc to (v from))) v mapping)
     (+ pos skip-size length)
     (inc skip-size)]))

(comment
  (reductions hash-transition [(vec (range 0 5)) 0 0] example))

(defn knot-hash-step [input [v pos skip-size :as state] ]
  (reduce hash-transition state input))

(defn checksum [n input]
  (apply * (take 2 (first (knot-hash-step input [(vec (range n)) 0 0])))))

(fact "checksum example"
  (checksum 5 example)
  => 12)

;;; part 2

(defn to-ascii [x]
  (concat (mapv int (str/join "," x))
          [17, 31, 73, 47, 23]))

(fact "ascii example"
  (to-ascii [1 2 3])
  => [49 44 50 44 51 17 31 73 47 23])

(defn knot-hash [n input]
  (->> (nth (iterate (partial knot-hash-step (to-ascii input)) [(vec (range n)) 0 0]) 64)
       first
       (partition 16)
       (map (fn [block] (format "%02x" (reduce bit-xor block))))
       str/join))

(fact "knot-hash example"
  (knot-hash 256 [1 2 3])
  => "3efbe78a8d82f29979031a4aa0b16a9d")

{:part-1 (checksum 256 input)
 :part-2 (knot-hash 256 input)}
