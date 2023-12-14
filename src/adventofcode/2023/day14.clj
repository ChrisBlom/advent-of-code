(ns adventofcode.2023.day14)

(def ex "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(defn parse [x]
  (mapv vec (clojure.string/split-lines x)))

(defn transpose [m]
  (apply mapv (comp vec list) m))


(defn move-line-right [line]
  (->> line
       (partition-by #{\#})
       (mapcat sort)))

(defn move-down [g]
  (->> g
       (transpose)
       (map move-line-right)
       (transpose)))

(defn move-up [g]
  (->> g
       reverse
       move-down
       reverse))

(defn move-right [g]
  (->> g
       (map move-line-right)))

(defn move-left [g]
  (->> g
       (map (comp reverse move-line-right reverse))))

(defn pp [g]
  (doseq [l g]
    (println (apply str l))))

(defn total-load [g]
  (->> (map-indexed (fn [y line]
                              (let [rounded (count (filter #{\O} line))
                                    row (- (count g) y)]
                                (* row rounded)))
                            g)
       (reduce + 0)))

(defn part-1 [input]
  (total-load (move-up (parse input))))

(def N 1000000000)


(assert (= (-> ex
               parse
               move-cycle
               move-cycle
               move-cycle)
           (parse ".....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#...O###.O
#.OOO#...O")
           ))


(defn move-cycle [g]
  (-> g
      move-up
      move-left
      move-down
      move-right))

(defn find-cycle [init-g]
  (loop [g init-g
         seen {}
         i 0]
    (cond
      (seen g)
      {:cycle-start (seen g)
       :cycle-end i
       :grid g
       :seen seen}
      (= i (dec N))
      (assert false "didn find cycle")
      :else
      (recur (move-cycle g)
             (assoc seen g i)
             (inc i)))))

(defn part-2 [input]
  (let [init-g (parse input)
        {:keys [cycle-end cycle-start  grid seen] :as r} (find-cycle init-g)
        cycle-len (- cycle-end cycle-start)
        remaining (mod (- N cycle-start) cycle-len)
        end-index (+ cycle-start remaining )]
    (total-load ((clojure.set/map-invert seen) end-index))))

(assert (= (part-1 ex) 136))
(assert (= (part-2 ex) 64))

{:part-1 (part-1 (user/day-input))
 :part-2
 (-/jfr-record
   (part-2 (user/day-input)))}
