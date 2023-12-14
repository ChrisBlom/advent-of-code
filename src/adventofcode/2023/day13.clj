(ns adventofcode.2023.day13)

(defn parse [s]
  (map vec (clojure.string/split-lines s)))

(def ex "#.##..##.")


(defn mirror [midpoint x]
  (- midpoint (- x midpoint)))

(defn find-midpoint [s]
  (let [s (vec s)]
    (into (sorted-set)
          (remove nil?
                  (for [offset (range 1 (count s))
                        :let [l (reverse (map s (range 0 offset)))
                              r (map s (->> (range offset (min (count s) (* offset 2)))))]]
                    (when (and (seq l) (seq r)
                               (every? identity
                                       (map = l r)))
                      offset))))))


(find-midpoint (vec "##.##.#"))

(find-midpoint (vec ".."))
(find-midpoint (vec "XYZZYX"))

(find-midpoint (vec "ZZZXYYX"))

(find-midpoint (vec  "#.##..##."))

(find-midpoint (apply str [\# \# \. \. \# \. \. \. \# \# \#]))

(def example "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.")

(def example-v "#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defn parse [in]
  (->> in
       clojure.string/split-lines
       (map vec)
       vec))

(defn find-h-splits [grid]
  (->> grid
       (map find-midpoint)
       (reduce (fn [acc mps]
                 (let [nacc (if acc
                              (clojure.set/intersection acc mps)
                              mps)]
                   (if (seq nacc)
                     nacc
                     (reduced nacc)))))))

(defn transpose [m]
  (apply mapv (comp vec list) m))

(defn find-v-splits [e]
  (find-h-splits (transpose e)))

(parse example)
(find-h-splits (parse  example))
(find-v-splits (parse  example-v))
(find-h-splits (transpose (parse example-v)))

(defn score [x]
  (+
   (or (some-> x find-h-splits first) 0)
   (or (some-> x find-v-splits first (* 100)) 0)))

(def e "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
")

{:part-1
 (->> (clojure.string/split (user/day-input) #"\n\n")
      (mapv parse)
      (map score)
      (reduce +)
      )}

(defn flip [x]
  (case x
    \# \.
    \. \#))


(defn smudged [g]
  (for [y (range 0 (count g))
        x (range 0 (count (g y)))]
    (update-in g [y x] flip)))

(defn score-2 [g]
  (let [v (find-v-split g)
        h (find-h-split g)
        new-h (first (remove #{h} (remove nil?  (mapcat find-h-splits (smudged g)))))
        new-v (first (remove #{v} (remove nil? (mapcat find-v-splits (smudged g)))))]
    (+
     (or new-h 0)
     (* 100 (or new-v 0)))))


{:part-2
 (->> (clojure.string/split (user/day-input) #"\n\n")
      (mapv parse)
      (map score-2)
      (reduce +)
      )}


1{:part-2 }

(defn pp [g]
  (println "---"
           )
  (doseq [ [i line] (map list (range) g)]
    (println (apply str  (map  {\. "⬜"
                                \# "⬛" } line))
             " | " (inc i)
             ))

  )



(pp (parse "..##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#."))
