(ns adventofcode.2019.day03
  (:require [midje.sweet :refer :all :except any?]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [line]
  (->> line
      (format "[%s]")
      read-string
      (map str)))

(defn parse [input]
  (map parse-line (str/split-lines input)))

(def ex1 (parse "U7,R6,D4,L4
  R8,U5,L5,D3"))

(def ex2 (parse "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"))

(def ex3 (parse "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))

(def input
  (->> "2019/day03.txt"
       io/resource
       slurp
       str/trim
       parse))

(defn delta [direction]
  (case direction
    \R [1 0]
    \L [-1 0]
    \U [0 1]
    \D [0 -1]))

(defn step-coordinates [ [x y :as start] step]
  (let [steps (Long/parseLong (subs step 1))
        direction (delta (first step))
        move (fn [x] (mapv + x direction))]
    (vec (take steps (rest (iterate move start))))))

(fact
  (step-coordinates [0 0] "R2") => [[1 0] [2 0]]
  (step-coordinates [0 0] "L2") => [[-1 0] [-2 0]]
  (step-coordinates [0 0] "U2") => [[0 1] [0 2]]
  (step-coordinates [0 0] "D2") => [[0 -1] [0 -2]] )

(defn path-coordinates [path]
  (reduce
   (fn [coords step]
     (into coords (step-coordinates (peek coords) step)))
   [[0 0]]
   path))

(defn abs [x]
  (if (neg? x) (- x) x))

(defn manhattan-distance [[x y]]
  (+ (abs x) (abs y)))

(fact
  (manhattan-distance [2 2]) => 4
  (manhattan-distance [-2 -2]) => 4)

(defn intersections [paths-coordinates]
  (let [skip-origin rest]
    (->>
     paths-coordinates
     (map (comp set skip-origin))
     (apply clojure.set/intersection))))

(defn nearest [intersections]
  (->>
   intersections
   (map manhattan-distance)
   (apply min)))

(defn nearest-intersection [input]
  (->> input
       (map path-coordinates)
       intersections
       nearest))

(fact
  (nearest-intersection ex1)
  => 6)

(defn wire-length [coordinate path]
  (reduce
   (fn [len pos]
     (if (= pos coordinate)
       (reduced len)
       (inc len)))
   0
   path))

(fact
  (wire-length [6 5] (path-coordinates (parse-line "U7,R6,D4,L4")))
  => 15)

(defn fewest-combined-steps [input]
  (let [paths (map path-coordinates input)
        sum-of-wire-lengths (fn [intersection] (apply + (map (partial wire-length intersection) paths)))]
    (->>
     paths
     intersections
     (map sum-of-wire-lengths)
     (apply min))))

(fact "part 2 examples"
  (fewest-combined-steps ex1) => 30
  (fewest-combined-steps ex2) => 610
  (fewest-combined-steps ex3) => 410)

{:part-1 (nearest-intersection input)
 :part-2 (fewest-combined-steps input)}
