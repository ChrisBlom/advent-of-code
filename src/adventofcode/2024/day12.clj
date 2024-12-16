(ns adventofcode.2024.day12
  (:require
   [adventofcode.utils :as u :refer [up down left right v+]]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "AAAA
BBCD
BBCC
EEEC
")

(defn parse [x]
  (mapv vec (str/split-lines x)))

(parse ex)

(defn in-bound? [g [y x]]
  (and (>= y 0)
       (< y (count g))
       (>= x 0)
       (< x (count (g 0)))))

(defn floodfill [g pos]
  (let [v (get-in g pos)]
    (loop [todo [pos]
           seen #{}]
      (if-some [p (first todo)]
        (let [pv (get-in g p)]
          (if (or (nil? pv)
                (seen p)
                (not= v pv))
            (recur (rest todo) seen)
            (recur (concat
                    (rest todo)
                    (remove seen (filter (partial in-bound? g) (u/grid-neighbours p))))
                   (conj seen p))))
        seen))))

(floodfill (parse ex) [0 0])

;; TODO wrong, use floodfill
(defn split-regions [g]
  (loop [todo (set (u/grid-positions g))
         regions []]
    (if-some [ pos (first todo)]
      (let [region-pos (floodfill g pos)]
        (recur (clojure.set/difference todo region-pos)
               (conj regions
                     {:label (get-in g pos)
                      :positions region-pos})))
      regions
      )))


(def a *1)

(defn plot-perimeter  [g label pos]
  (count
   (for [n (u/grid-neighbours pos)
         :let [nv (get-in g n)]
         :when (not= nv label)]
     nv)))

(plot-perimeter [[0 0 0]
                 [1 0 1]
                 [0 0 0]] 0 [1 1])

(defn region-perimiter [g {:keys [label positions]}]
  (->> positions
       (map #(plot-perimeter g label %))
       (reduce +)))

(defn score [g]
  (let [regions (split-regions g)
        perims (map (partial region-perimiter g) regions)
        areas  (map (comp count :positions) regions)]
    (reduce + (map * perims areas))
#_    (->>
       (map * perims areas)
       (reduce +))))

(score (parse ex))


(score (parse "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"))



(defn part-1 [input]
  (let [x (parse input)]
    (score x)))

(assert (= 140 (part-1 ex)))


(assert (= 772 (part-1 "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
")))

(assert (= 1930 (part-1 "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")))

(defn part-2 [input]
  (let [x (parse input)]
    (score )
    ))





{:part-1 (part-1 (user/day-input))
 :part-2 (time (part-2 (user/day-input)))}


(comment



  )
