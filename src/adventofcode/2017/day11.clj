(ns adventofcode.2017.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [midje.sweet :refer :all]))

(def input (str/trim (slurp (io/resource "2017/day11.txt"))))

;;      n = (+ x y)
;;         :
;; [1 0 0] :    [0 1 0]
;;     x   :      y
;;     \   v    /
;;      \  n  /
;;    nw +--+ ne  <..... ne = (+ y z)
;;      /    \
;;    -+      +--- z [ 0 0 1]
;;      \    /
;;    sw +--+ se  <..... se = (+ z -x
;;      / s  \
;;            \
;;            -x

(def basis
  [[1 0 0] ; x
   [0 1 0] ; y
   [0 0 1] ; z
   ])

(def compass-point->cube-coordinate
  (let [;; base directions for a full circle
        all-base-directions (concat basis (map (partial mapv -) basis))
        ;; the cube coordinate is the sum of the adjacent two base directions
        directions (map (partial mapv +) all-base-directions (rest (cycle all-base-directions)))]
    (zipmap '[n ne se s sw nw]
            directions)))

(defn abs [x] (if (neg? x) (- x) x))

(defn distance [pos] (apply max (map abs pos)))


(defn solution [x]
  (let [distances  (->> (format "[%s]" x)
                        read-string
                        (map compass-point->cube-coordinate)
                        (reductions (partial map +))
                        (map distance))]
    {:part-1 (last distances)
     :part-2 (apply max distances)}))

(fact "works for examples"
  (:part-1 (solution "ne,ne,ne")) => 3
  (:part-1 (solution "ne,ne,sw,sw")) => 0
  (:part-1 (solution "ne,ne,s,s")) => 2
  (:part-1 (solution "se,sw,se,sw,sw")) => 3)

(solution input)
