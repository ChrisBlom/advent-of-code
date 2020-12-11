(ns adventofcode.2020.day11
    (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn parse-seats [s]
  (mapv vec (str/split-lines s)))


(defn print-seats [state]
  (println "seats:")
  (run! (comp println (partial str/join " ")) state))

(def example
  (parse-seats "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
"))

(def directions
  (for [dy (range -1 2)
        dx (range -1 2)
        :when (not (= dy dx 0))]
    [dy dx]))

(defn adjacent [prev-state y x]
  (remove nil?
          (map (fn [ [dy dx] ]
                 (get-in prev-state [(+ y dy) (+ x dx)]))
               directions)))

(defn occupied? [x]
  (= x \#))

(def seat? #{\# \L})

(defn occupied-adjacent-seats [state y x]
  (count (filter occupied? (adjacent state y x))))

(assert (= 5 (occupied-adjacent-seats [(vec ".#.")
                                       (vec "###")
                                       (vec ".##")
                                       ]
                                      1 1)))

(defn update-cell [prev-state y x]
  (case (get-in prev-state [y x])
    \L (if (= (occupied-adjacent-seats prev-state y x) 0)
         \#
         \L)
    \# (if (>= (occupied-adjacent-seats prev-state y x) 4)
         \L
         \#)
    \. \.))


(update-cell [(vec ".#.")
              (vec "###")
              (vec "...")]
             1 1)

(defn apply-rules [update-cell state]
  (->> state
       (map-indexed (fn [y row]
                      (->> row
                           (map-indexed (fn [x seat] (update-cell state y x)))
                           vec)))
       vec))

(defn fixpoint [f x]
  (loop [x x]
    (let [nx (f x)]
      (if (= nx x)
        x
        (recur nx)))))

(defn count-occupied [rows]
  (count (filter occupied? (apply concat rows))))

(assert (= 37 (count-occupied (fixpoint (partial apply-rules update-cell) example))))

{:part-1
 (-> (count-occupied (fixpoint (partial apply-rules update-cell) (mapv vec (str/split-lines (slurp (io/resource "2020/day11.txt")))))))}

2359

(defn visible-occupied-seats [state y x]
  (->> directions
       (map (fn [direction]
              (->> (iterate #(mapv + direction %) [y x])
                   rest
                   (map #(get-in state %))
                   (take-while some?)
                   (filter seat?)
                   first)))
       (filter occupied?)
       count))

(assert
 (= 8
    (visible-occupied-seats
     (parse-seats
      ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....")
     4 3)))

(assert
 (= 0 (visible-occupied-seats
       (parse-seats ".##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##.")
       3 3)))

(assert
 (= 0
    (visible-occupied-seats
     (parse-seats ".............
.L.L.#.#.#.#.
.............")
     1 1
     )))

(defn update-cell-p2 [prev-state y x]
  (case (get-in prev-state [y x])
    \L (if (= (visible-occupied-seats prev-state y x) 0)
         \#
         \L)
    \# (if (>= (visible-occupied-seats prev-state y x) 5)
         \L
         \#)
    \. \.))
(def example-sequence
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL

#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##

#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#

#.L#.##.L#
#L#####.LL
L.#.#..#..
##L#.##.##
#.##.#L.##
#.#####.#L
..#.#.....
LLL####LL#
#.L#####.L
#.L####.L#

#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##LL.LL.L#
L.LL.LL.L#
#.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLL#.L
#.L#LL#.L#

#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.#L.L#
#.L####.LL
..#.#.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#

#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.LL.L#
#.LLLL#.LL
..#.L.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#")

(assert
 (every? true?
         (map =
              (iterate (partial apply-rules update-cell-p2 ) (first
                                                              (map parse-seats (str/split example-sequence #"\n\n"))))

              (map parse-seats (str/split example-sequence #"\n\n")))))

{:part-2
 (-> (count-occupied (fixpoint (partial apply-rules update-cell-p2) (parse-seats (slurp (io/resource "2020/day11.txt"))))))}
