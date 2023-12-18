q(ns adventofcode.2023.day18
  (:require [clojure.string :as str]))

(def example-plan
  "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")


(defn parse-line [line]
  (let [ [_ dir steps color] (re-matches #"([A-Z]) (\d+) \((.+)\)" line)]
    [dir (parse-long steps) color]))

(defn parse [s]
  (map parse-line (str/split-lines s)))

(def d->vec
  {"R" [0 1]
   "L" [0 -1]
   "U" [-1 0]
   "D" [1 0]})

(def vec->d
  (clojure.set/map-invert d->vec))

(defn step [dir color [pos grid] ]
  [(mapv + pos (d->vec dir))
   (assoc grid pos color)])

(defn dig [plan]
  (second
   (reduce
    (fn [ [pos grid] [dir steps color]]
      (->> [pos grid]
           (iterate (partial step dir color))
           (drop steps)
           first))
    [ [0 0] (sorted-map) ]
    plan)))

(def trench-ex "#######
#.....#
###...#
..#...#
..#...#
###.###
#...#..
##..###
.#....#
.######")




(defn ppg [s]
  (let [g (if (string? s) (parse s) s)
        g (if (set? g) (zipmap g g) g)
        y-min (apply min (map first (keys g)))
        y-max (apply max (map first (keys g)))
        x-min (apply min (map second (keys g)))
        x-max (apply max (map second (keys g)))]
    (doseq [y (range y-min (inc y-max))]
      (println
       (apply str
              (for [x (range x-min (inc x-max))]
                (if-let [c (g [y x])]
                  "#"
                  ".")))))))

(assert
 (= (str/split-lines trench-ex)
    (str/split-lines
     (with-out-str
       (ppg
        (dig (parse example-plan)))))))

(defn flood-fill [boundary]
  (let [min-x (apply min (map second (keys boundary)))
        min-y (apply min (map first (keys boundary)))
        max-x (apply max (map second (keys boundary)))
        max-y (apply max (map first (keys boundary)))
        outside-edges   (concat
                         (for [y (range min-y (inc max-y))
                               x [(dec min-x) (inc max-x)]]
                           [y x] )
                         (for [x (range min-x (inc max-x))
                               y [(dec min-y) (inc max-y)]
                               ]
                           [y x])       ; right edge
                         )
        in-bounds? (fn [ [y x]]
                     (and (<= min-y y max-y)
                          (<= min-x x max-x)))]
    (loop [todo outside-edges
           visited (transient #{})]
      (if-some [ pos (first todo)]
        (if (visited pos)
          (recur (rest todo) visited)
          (let [next-positions (->> (vals d->vec)
                                    (map #(mapv + pos %))
                                    (filter in-bounds?)
                                    (remove visited)
                                    (remove boundary))]
            (recur
             (concat next-positions (rest todo))
             (if (in-bounds? pos)
               (conj! visited pos)
               visited
               ))))
        (persistent! visited)))))

(defn grid-size [grid]
    (let [min-x (apply min (map second (keys grid)))
          min-y (apply min (map first (keys grid)))
          max-x (apply max (map second (keys grid)))
          max-y (apply max (map first (keys grid)))]
      {:w (- (inc max-x) min-x)
       :h (- (inc max-y) min-y)}))

(defn part-1 [input]
  (let [plan (parse input)
        trench (dig plan)
        outside-trench (flood-fill trench)
        {:keys [w h]} (grid-size trench)]
    (- (* w h) (count outside-trench))))

(assert (= 62 (part-1 example-plan)))

(defn convert-hex [ [_ _ hex-color]]
  (let [steps (read-string (str/replace (subs hex-color 0 6) "#" "0x"))
        dir (case (last hex-color)
              \0 "R"
              \1 "D"
              \2 "L"
              \3 "U")]
    [dir steps]))

(assert (= (convert-hex ["R" 1 "#411b91"])
           ["D" 266681]))

(defn scale [ v s]
  [(* s (v 0))
   (* s (v 1))])

(defn dig-hull [plan]
  (reductions
   (fn [ pos [dir steps color]]
     (mapv + pos (scale (d->vec dir) steps)))
   [0 0]
   plan))

(comment

  (ppg (set (dig-hull (parse example-plan))))

  (ppg (dig (parse example-plan)))

  (ppg (dig (parse (user/day-input)))))

(defn shoelace [ hull]
  (-> (- (reduce + (map *
                        (map first hull)
                        (map second (rest(cycle hull)))))
         (reduce + (map *
                        (map second hull)
                        (map first (rest (cycle hull))))))
      (abs)
      (/ 2)))

;; random example from a youtube video
(assert (= 29 (shoelace '[[-2 4] [-4 -1] [1 -3] [3 2] [-2 4]])))

(defn part-1-fast [plan]
  (let [hull (dig-hull plan)]
    (+ (shoelace hull)
       (/ (reduce + (map second plan)) 2)
       1)))

(assert (= 62 (part-1-fast (parse example-plan))))

(defn part-2 [input]
  (part-1-fast (map convert-hex (parse input))))

(assert (= 952408144115 (part-2 example-plan)))

{:part-1 (part-1 (user/day-input))
 :part-2 (part-2 (user/day-input))}
