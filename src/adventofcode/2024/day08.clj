(ns adventofcode.2024.day08
  (:require
   [adventofcode.utils :as u]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
")



(defn parse [x]
  (mapv vec (str/split-lines x))
  )


(defn in-bound? [g [y x]]
  (and (>= y 0)
       (< y (count g))
       (>= x 0)
       (< x (count (g  0)))))

(defn antenna-groups [grid]
  (let [lp (keep (fn [pos]
                   (let [label (get-in grid pos)]
                     (if (not= \. label)
                       [label pos])))
                 (u/grid-positions grid))]
    (reduce (fn [acc [label pos]]
              (update acc label (fnil conj #{}) pos))
            {}
            lp)))

(parse ex)

(antenna-groups (parse ex))

(defn v- [a b] (mapv - a b))
(defn v+ [a b] (mapv + a b))
(defn -v [a] (mapv - a))

(defn antinode [a b]
  (when (not= a b)
    (let [a->b (v- b a)]
      [(v+ b a->b)])))

(defn antinode-2 [g a b]
  (when (not= a b)
    (let [a->b (v- b a)]
      (take-while (partial in-bound? g)
                  (iterate
                   (partial v+ a->b)
                   b)))))

(defn antinodes-1 [grid as]
  (for [a as
        b as
        an (antinode a b)
        :when (in-bound? grid an)
        :when (not (contains? as an))]
    an))

(defn antinodes-2 [grid as]
  (for [a as
        b as
        an (antinode-2 grid a b)
        :when (in-bound? grid an)
        ;:when (not (contains? as an))
        ]
    an))

(let[ g (parse ex)
      a (get (antenna-groups g) \A)]
  (antinodes-1 g a ))

(let[ g (parse ex)
      a (get (antenna-groups g) \A)]
  (antinodes-2 g a ))



(in-bound? (parse "..\n..") [0 0] )
(in-bound? (parse "..\n..") [0 2] )


(defn pp [in anti-nodes]
  (let [g (parse in)
        antis (for [[label ps] (antenna-groups g)
                    an (anti-nodes g ps)]
                an)

        g- (reduce (fn [acc p]
                     (update-in acc p
                                (fn [v]
                                  (if (not= v \.)
                                    v
                                    \#))))
                  g
                  antis)]

    (doseq [line g-]
      (println (str/join line)))))

(pp ex antinodes-1)
(pp ex antinodes-2)


(assert (=
         (with-out-str (pp ex antinodes-1))
         "......#....#
...#....0...
....#0....#.
..#....0....
....0....#..
.#....A.....
...#........
#......#....
........A...
.........A..
..........#.
..........#.
"))

(assert (=
         (with-out-str (pp ex antinodes-2))
         "##....#....#
.#.#....0...
..#.#0....#.
..##...0....
....0....#..
.#...#A....#
...#..#.....
#....#.#....
..#.....A...
....#....A..
.#........#.
...#......##
"))


(defn part-1 [input]
  (let [g (parse input)
        groups (antenna-groups g)
        antis (for [[label ps] groups
                    an (antinodes-1 g ps)]
                an)]
    (count (set antis))))

(defn part-2 [input]
  (time
   (let [g (parse input)
         groups (antenna-groups g)
         antis (for [[label ps] groups
                     an (antinodes-2 g ps)]
                 an)]
     (count (set antis)))))

(assert (= 34 (part-2 ex)))


{:part-1 (part-1 (user/day-input))
 :part-2 (time (part-2 (user/day-input)))}


(comment



  )
