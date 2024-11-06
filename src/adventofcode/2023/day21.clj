(ns adventofcode.2023.day21
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.test :as t]))


(def ex
  "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")


(defn map-2d [f-yxv grid]
  (map-indexed (fn [y line]
                 (map-indexed (fn [x v] (f-yxv y x v)) line))
               grid))

(defn parse [ex]
  (let [lines (str/split-lines ex)]
    {:w (count (first lines))
     :h (count lines)
     :g
     (into (sorted-map)
           (apply concat
                  (map-2d (fn [y x c] [[y x] c]) lines)))}))

(defn start-pos [g]
  (ffirst   (filter (comp #{\S} val) g)))

(def up [-1 0])
(def down [1 0])
(def right [0 1])
(def left [0 -1])
(def dirs [up down left right])

(defn reachable-next-step [g positions]
  (distinct (for [p positions
                 d dirs
                 :let [np (mapv + p d)]
                 :when (#{\S \.} (g np))]
             np)))


(let [g (parse ex)
      s (start-pos g)]
  (count (first (drop 6
                      (iterate (partial reachable-next-step g) [s])))) )



(defn ppg [g reachables]
  (let [reachables (set reachables)
        y-min (apply min (map first (keys g)))
        y-max (apply max (map first (keys g)))
        x-min (apply min (map second (keys g)))
        x-max (apply max (map second (keys g)))]
    (doseq [y (range y-min (inc y-max))]
      (println
       (apply str
              (for [x (range x-min (inc x-max))
                    :let [l [y x]]]
                (if (reachables l)
                  "0"
                  (g l))))))))

(let [g (parse ex)
      s (start-pos g)
]

  (doseq [i (range 7)]
    (println  i)
    (let [r (first (drop i (iterate (partial reachable-next-step g) [s])))]
      (ppg g r)
      (println (count r))
      ))
  )


(defn part-1 [input n]
  (let [g (:g (parse input))
        s (start-pos g)]
    (count (first (drop n (iterate (partial reachable-next-step g) [s]))))))


(part-1 ex 6)

(part-1 (user/day-input) 64)


(defn bounded [w h pos]
  [(mod (pos 0) h)
   (mod (pos 1) w)])


(defn reachable-next-step-repeating [g w h positions]
  (into #{}
        (for [p positions
              d dirs
              :let [np (mapv + p d)]
              :when (#{\S \.} (g (bounded w h np)))]
          np)))

(defn reachable-next-step-in-grid [g w h positions]
  (set (for [p positions
             d dirs
             :let [ [y x :as np] (mapv + p d)]
             :when (and (<= 0 y) (< y h))
             :when (and (<= 0 x) (< x w))
             :when (#{\S \.} (g np))]
         np)))



(let [{:keys [g w h]} (parse (user/day-input))]
  (loop [p [(start-pos g)]
         i 0
         seen {}]
    (println i (count p))
    (when (< i 400)
      (if (contains? seen (count p))
        seen
        (recur (reachable-next-step-in-grid g w h p)
               (inc i)
               (assoc seen i (count p)))))))


(ppg (:g (parse (user/day-input)))
     ((clojure.set/map-invert _seen) 400))

(defn part-2 [input]
  (let [{:keys [g w h]} (parse input)
        s (start-pos g)]
    (map (fn  f [a b]
           [a (count b)])
         (range)
         (iterate (partial reachable-next-step-repeating g w h) [s]))))



(defn part-2 [input]
  (let [{:keys [g w h]} (parse input)
        s (start-pos g)]
    (map (fn  f [a b]
           [a (count b)])
         (range)
         (iterate (partial reachable-next-step-repeating g w h) [s]))))


(defn )



(ppg (:g  (parse (user/day-input))) #{})
