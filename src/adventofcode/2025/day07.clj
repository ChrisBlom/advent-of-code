(ns adventofcode.2025.day07
  (:require
   [adventofcode.utils :as u]
   [clojure.string :as str]))

(def ex ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
")


(defn parse [ex]
  (mapv vec (str/split-lines ex)))

(parse ex)

(defn start-pos [lines]
  [0 (first (keep-indexed (fn [i v] (if (= v \S) i)) (first lines)))])

(start-pos (parse ex))

(def down [1 0])
(def right [0 1])
(def left [0 -1])

(defn pr-grid [g]
  (println "----------------------------------------")
  (doseq [line g]
    (println (apply str line))))

(defn count-splits [grid]
  (loop [todo [ (start-pos grid) ]
         grid grid
         splits #{}]
    #_(pr-grid grid)
    (if-let [pos (first todo)]
      (let [v (get-in grid pos)]
        (case v
          \^
          (recur
           (concat [(mapv + pos left  down)
                    (mapv + pos right down)]
                   (rest todo))
           grid #_(assoc-in grid pos (inc (count splits)))
           (conj splits pos))
          (\S \.)
          (recur
           (concat [(mapv + pos down)] (rest todo))
           (assoc-in grid pos \|)
           splits)
          (nil \|)
          (recur
           (rest todo)
           grid
           splits)))
      {:splits splits
       :grid grid})))

(defn part-1 [ex] (count (:splits (count-splits (parse ex)))))

(defn part-2 [input]
  (let [grid (parse input)
        count-options
        (u/fn-memoized solve [pos]
          (let [v (get-in grid pos)]
            (case v
              \^
              (+ (solve (mapv + pos left down))
                 (solve (mapv + pos right down)))
              (\S \.) (solve (mapv + pos down))
              nil 1
              )))]
    (count-options (start-pos grid))))

(assert (= 40 (part-2 ex)))

{:part-1 (part-1 (user/day-input))
 :part-2 (part-2 (user/day-input))}
