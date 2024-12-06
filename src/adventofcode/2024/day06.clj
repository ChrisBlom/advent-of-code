(ns adventofcode.2024.day06
  (:require
   [adventofcode.utils :as u]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defn parse [x]
  (mapv vec (str/split-lines x)))

(parse ex)

(defn start-pos [grid]
  (first
   (filter
    (fn [pos] (= \^ (get-in grid pos)))
    (u/grid-positions grid))))

(start-pos (parse ex))

(defn v+ [a b]
  [(+ (a 0) (b 0))
   (+ (a 1) (b 1))])

(defn start-state [grid]
  {:pos (start-pos grid)
   :dir u/up
   :visited #{}})

(defn step [grid {:keys [pos dir visited] :as s}]
  (let [npos (v+ pos dir)
        at-npos (get-in grid npos)
        ;; track pos,dir for cycle detection
        state-key [pos dir]
        nvisited (conj visited state-key)]
    (if (visited state-key) ;; we enter a loop if we visit a position twice with the same direction
      {:done true :loop true}
      (case at-npos
        nil (-> s
                (assoc :done true
                       :visited nvisited))
        \# (-> s
               (update :dir u/rot-right)
               (assoc :visited nvisited))
        (-> s
            (assoc :pos npos)
            (assoc :visited nvisited))))))

(defn take-while+
  "like `take-while` but includes the first
   item where (pred item) does not return logical true"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) (take-while+ pred (rest s)))
       (list (first s))))))

(defn walk-path [step state grid]
  (->> state
       (iterate (partial step grid))
       (take-while+ (complement :done))))

(defn part-1 [input]
  (let [grid (parse input)]
    (->> grid
         (walk-path step (start-state grid))
         last
         :visited
         (map first) ; pos
         distinct
         count)))

(assert (= 41 (part-1 ex)))

(defn window-pairs [coll]
  (map list coll (rest coll)))

(defn part-2 [input]
  (let [grid (parse input)
        start (start-state grid)
        walked (walk-path step start grid)]
    (->>
     (window-pairs walked)
     ;; only process first occurance of insert pos
     ;; to reduce work and avoid inserting a # in an already visited position
     (u/distinct-by (comp :pos second))
     (pmap (fn [ [prev-state next-state] ]
                   (let [insert-pos (:pos next-state)]
                     (and
                      ;; can't insert # if it already there
                      (not= \# (get-in grid insert-pos))
                      ;; check if walk ends in loop
                      (:loop
                       (last (walk-path
                              step
                              ;; reuse state from initial walk,
                              prev-state
                              (assoc-in grid insert-pos \#))))))))
     (filter identity)
     count)))

(assert (= 6 (part-2 ex)))

{:part-1 (part-1 (user/day-input))
 :part-2 (time (part-2 (user/day-input)))}


(comment

  )
