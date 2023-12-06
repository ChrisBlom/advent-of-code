(ns adventofcode.2023.day05
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.java.io :as io]))

(def example
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn mkr [start end]
  {:pre [(< start end)]}
  [start end])

(defn in-range? [ [start end] x]
  (and (<= start x) (< x end)))

(defn parse-ranges [range-str]
  (let [ [ dest-start  source-start length]  (read-string (str "["  range-str "]"))]
    {:source-range (mkr source-start (+ source-start length))
     :delta (- dest-start source-start) }))

(defn parse-map [ [label & ranges]]
  (let [label (first (str/split label #" "))]
    [(keyword label) (mapv parse-ranges ranges)]))

(defn parse [input]
  (let [ [seeds & maps] (->> input
                             str/split-lines
                             (partition-by #{""})
                             (remove #{[""]}))
        seeds  (->> (str/split (first seeds) #":")
                    second
                    (format "[%s] ")
                    read-string)]
    {:seeds seeds
     :maps (into (array-map)  (map parse-map maps))}))

(defn source->dest [x maps]
  (if-some [{:keys [source-range delta]} (first maps)]
    (if (in-range? source-range x)
      (+ x delta)
      (recur x (rest maps)))
    x))

(assert (= 50 (source->dest 98 (-> example parse :maps first val))))
(assert (= 51 (source->dest 99 (-> example parse :maps first val ))))
(assert (= 10 (source->dest 10 (-> example parse :maps first val ))))
(assert (= 50 (source->dest 98 (-> example parse :maps first val ))))
(assert (= 55 (source->dest 53 (-> example parse :maps first val ))))

(defn part-1 [input]
  (let [{:keys [seeds maps]} (parse input)]
    (->> (for [s seeds]
           (reduce source->dest s (vals maps)))
         (apply min))))

(assert (= 35 (part-1 example)))

(defn seed-ranges [ seeds]
  (for [ [start len] (partition 2 seeds)]
    (mkr start (+ start len))))

(defn shift-range [ [start end] delta]
  [(+ start delta)
   (+ end delta)])

(defn source->dest-ranges [ in-range maps]
  (if-some [ {:keys [ source-range delta]} (first maps)]
    (let [[in-start in-end] in-range
          [source-start source-end] source-range ]
      (cond
        ;; no overlap
        (or (<= in-end source-start)
            (>= in-start source-end))
        (recur in-range (rest maps))

        ;; in-range contained in source range
        (and (>= in-start source-start)
             (<= in-end source-end))
        [(shift-range in-range delta)]

        ;;            |----------in-------------|
        ;;    |-------source------|
        ;; ........................................
        ;;            | overlap   | remainder   |
        (and (>= in-start source-start)
             (> in-end source-end))
        (let [overlap (mkr in-start source-end)
              remainder (mkr source-end in-end)]
          (cons (shift-range overlap delta)
                (source->dest-ranges remainder (rest maps))))

        ;; |----------in-------------|
        ;;               |--source------|
        ;; ........................................
        ;; | remainder  | overlap   |
        (and (< in-start source-start)
             (>= in-end source-start))
        (let [overlap (mkr source-start in-end)
              remainder (mkr in-start source-start)]
          (cons (shift-range overlap delta)
                (source->dest-ranges remainder (rest maps))))))
    ;; no match, keep as is
    [in-range]))



(assert (= [[81 82]] (source->dest-ranges [79 80] (val (first (:maps (parse example)))))))
(assert (= [[57 58]] (source->dest-ranges [55 56] (val (first (:maps (parse example)))))))

(defn part-2 [input]
  (let [{:keys [seeds maps]} (parse input)
        location-ranges (reduce (fn [ranges maps]
                                  (mapcat #(source->dest-ranges % maps) ranges))
                                (seed-ranges seeds)
                                (vals maps))]
    (->> location-ranges
         (map first)
         (apply min))))

(assert (= 46 (part-2 example)))

(let [input (slurp (io/resource "2023/day05.txt"))]
  (doto {:part-1 (part-1 input)
         :part-2 (part-2 input)}
    println))

;; {:part-1 199602917, :part-2 2254686}
