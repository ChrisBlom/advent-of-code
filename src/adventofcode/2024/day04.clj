(ns adventofcode.2024.day04
  (:require
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
")

(defn parse [x]
  (mapv vec (str/split-lines x)))

(defn positions [grid]
  (for [y (range 0 (count grid))
        x (range 0 (count (grid 0)))]
    [y x]))

(def up [-1 0])
(def down [1 0])
(def right [0 1])
(def left [0 -1])
(def  up-right            (mapv + up right))
(def  up-left            (mapv + up left))
(def  down-right            (mapv + down right))
(def  down-left            (mapv + down left))

(def dirs [up
           down
           left
           right
           up-left down-right down-left up-right
           ])

(defn find-word [grid dir word pos]
  (let [[y x] pos]
    (cond
      (empty? word)
      pos

      (= (first word) (get-in grid pos))
      (recur grid dir (rest word) (mapv + dir pos))

      :else false)))

(defn find-all [grid]
  (for [pos (positions grid)
        dir dirs]
    (find-word grid dir (seq "XMAS") pos)))

(defn part-1 [x]
  (count (filter identity (find-all (parse x)))))

(assert (= 18 (part-1 ex)))

(defn x-mas? [grid pos]
  (and (= \A (get-in grid pos))
       (or
        (and
         (= \M (get-in grid (mapv + pos up-left)))
         (= \S (get-in grid (mapv + pos down-right))))
        (and
         (= \S (get-in grid (mapv + pos up-left)))
         (= \M (get-in grid (mapv + pos down-right)))))
       (or
        (and
         (= \M (get-in grid (mapv + pos up-right)))
         (= \S (get-in grid (mapv + pos down-left))))
        (and
         (= \S (get-in grid (mapv + pos up-right)))
         (= \M (get-in grid (mapv + pos down-left)))))))

(defn find-x-mas [grid]
  (filter (partial x-mas? grid) (positions grid) ))

(defn part-2 [x]
  (count (find-x-mas (parse x))))

(assert (= 9 (part-2 ex)))

(let [input (user/day-input)]
  (time
   {:part-1 (part-1 input)
    :part-2 (part-2 input)}))
