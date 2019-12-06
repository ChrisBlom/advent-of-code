(ns adventofcode.2019.day06
  (:require
   [midje.sweet :refer :all :except any?]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]))

(defn parse [txt]
  (->> txt
       str/split-lines
       (map #(str/split % #"\)"))))

(def ex1 (parse "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"))

(def ex2
  (parse "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN"))

(def input
  (->> "2019/day06.txt"
       io/resource
       slurp
       parse))

(defn to-orbit-map [orbits]
  (reduce
   (fn [acc [around orbits]]
     (assoc acc orbits around))
   {}
   orbits))

(defn path-to-com [orbits obj]
  (->> obj
       (iterate orbits)
       (take-while #(not= % "COM"))))

(defn total-orbits-length [input]
  (let [orbits (to-orbit-map input)
        all-objects (distinct (mapcat identity input))
        orbits-to-com #(count (path-to-com orbits %))]
    (->> all-objects
         (map orbits-to-com)
         (apply +))))

(fact "part 1 example"
  (total-orbits-length ex1)
  => 42)

(fact "part 1"
  (total-orbits-length input)
  => 204521)

(defn minimun-transfers [input]
  (let [orbits (to-orbit-map input)
        path-from-santa (set (path-to-com orbits (get orbits "SAN")))
        path-from-you (set (path-to-com orbits (orbits "YOU")))]
    (count (set/difference (set/union path-from-santa path-from-you)
                           (set/intersection path-from-santa path-from-you)))))



(fact "part 2 example"
  (minimun-transfers ex2)
  => 4)

(fact "part 2"
  (minimun-transfers input)
  => 307)
