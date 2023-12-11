(ns adventofcode.2023.day11
  (:require [clojure.string :as str]))

(def example "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(defn parse [grid-string]
  (mapv vec (clojure.string/split-lines grid-string)))

(defn transpose [m]
  (apply mapv vector m))

(defn map-2d [f-yxv grid]
  (map-indexed (fn [y line]
                 (map-indexed (fn [x v] (f-yxv y x v)) line))
               grid))

(defn expand [g]
  (reduce (fn [acc x]
            (if (some #{\#} x)
              (conj acc x)
              (conj acc x x)))
          []
          g))

(defn galaxy-positions-naive [example]
  (->>
   example
   parse
   transpose
   expand
   transpose
   expand
   (map-2d (fn [y x v] [ [y x] v]))
   (apply concat)
   (filter (comp #{\#} second))
   (mapv first)
   sort
   vec))

(assert (= (galaxy-positions-naive example)
           [[0 4] [1 9] [2 0] [5 8] [6 1] [7 12] [10 9] [11 0] [11 5]]))

(defn derive-scale-map
  [scale grid]
  (->> grid
       (reductions
        (fn [ [y off-y] line ]
          [(inc y)
           (+ off-y (if (some #{\#} line) 1 scale))])
        [ 0 0])
       (into {})))

(defn galaxy-positions-scaled [scale example]
  (let [grid  (parse example)
        y-map (derive-scale-map scale grid)
        x-map (derive-scale-map scale (transpose grid))
        apply-scale (fn [ [y x]] [(y-map y) (x-map x)])]
    (->>
     grid
     (map-2d (fn [y x v] [ [y x] v]))
     (apply concat)
     (filter (comp #{\#} second))
     (map first)
     (map apply-scale))))

(assert
 (= (galaxy-positions-scaled 2 example ) (galaxy-positions-naive example)))

(defn galaxy-dist [ a b]
  (apply + (map (comp abs -) a b)))

(defn distance-sum [galaxy-positions-naive]
  (let [nodes (vec galaxy-positions-naive)
        shortes-dists (for [i (range 0 (count nodes))
                            j (range (inc i) (count nodes))]
                        (galaxy-dist (nodes i) (nodes j)))]
    (reduce + shortes-dists)))


(assert (= 374 (distance-sum (galaxy-positions-naive example))))
(assert (= 374 (distance-sum (galaxy-positions-scaled 2 example))))
(assert (= 1030 (distance-sum (galaxy-positions-scaled 10 example))))

(def input (user/day-input))

{:part-1 (distance-sum (galaxy-positions-naive input))
 :part-2 (distance-sum (galaxy-positions-scaled 1000000 input))}
