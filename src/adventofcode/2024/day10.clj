(ns adventofcode.2024.day10
  (:require
   [adventofcode.utils :as u]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "
...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9")

(defn parse [s]
  (->> s
       str/split-lines
       (remove str/blank?)
       (mapv (fn [l] (mapv (fn [c] (parse-long (str c))) l)))))

(parse ex)

(defn pp [grid]
  (doseq [line grid]
    (doseq [c line]
      (print (or c \.)))
    (println)))

(defn reachable-9s
  ([grid]
   (->>(u/grid-seq grid)
       (keep (fn [ [pos v] ] (when (and v (zero? v))
                              (reachable-9s grid pos 0))))))
  ([grid pos step-counter]
   (case step-counter
     9 [pos]
     (for [next-pos (u/grid-neighbours pos)
           :when (= (get-in grid next-pos) (inc step-counter))
           reachable (reachable-9s grid next-pos (inc step-counter))]
       reachable))))

(defn part-1 [i]
  (reduce + (map (comp count set) (reachable-9s (parse i)))))


(assert (= 4
           (part-1 "..90..9
...1.98
...2..7
6543456
765.987
876....
987....")))

(assert (= 3 (part-1 "10..9..
2...8..
3...7..
4567654
...8..3
...9..2
.....01")))

(part-1 "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
")


(defn part-2 [i]
  (reduce + (map count (reachable-9s (parse i)))))

(assert (= 3
           (part-2 "
.....0.
..4321.
..5..2.
..6543.
..7..4.
..8765.
..9....
")))

(assert
 (= 13 (part-2 "
..90..9
...1.98
...2..7
6543456
765.987
876....
987....")))

(assert
 (= 227  (part-2 "
012345
123456
234567
345678
4.6789
56789.
")))

(assert (= (reduce + [20, 24, 10, 4, 1, 4, 5, 8,  5])
           (part-2
            "
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")))

{:part-1 (time (part-1 (user/day-input)))
 :part-2 (time (part-2 (user/day-input)))}
