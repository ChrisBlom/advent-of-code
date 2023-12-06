(ns adventofcode.2023.day06
  (:require
   [clojure.string :as str]))

(def example
  "Time:      7  15   30
Distance:  9  40  200")

(defn parse [input]
  (let [ [times record-distances] (str/split-lines input)
        [_ & times] (map parse-long (str/split times #"Time:\ +|\ +"))
        [_ & record-distances] (map parse-long (str/split record-distances #"Distance:\ +|\ +"))]
    (map (fn [ t d] {:time t :distance d})
         times
         record-distances)))

(defn distance-travelled [time pressed]
  (* pressed (- time pressed)))

(defn ways-to-win [{:keys [time distance]}]
  (->> (map (partial distance-travelled time) (range 0 (inc time)))
       (filter #(> % distance))
       count))

(def input
  "Time:        46     68     98     66
Distance:   358   1054   1807   1080")

(defn join-times-and-distances [times+distances]
  {:time (parse-long (str/join (map :time times+distances)))
   :distance (parse-long (str/join (map :distance times+distances)))})

(defn solve-quadratic
  "returns a list of ways-to-win-analytic to x^2 + b x + c = 0"
  [ a b c ]
  (for [+- [- +]]
    (/ (+- (- b) (Math/sqrt (- (* b b)
                               (* 4 a c))))
       (* 2 a))))

(defn ways-to-win-analytic
  "let x = no. of steps pressed.
    we find the highest and lowest x for which the distance travelled is greater than the record distance
    solve: x * (time - x) > distance
       =  -x^2 + time*x - distance > 0
    to no. of possibilities = highest solution - lowest solution"
  [{:keys [time distance]}]
  (let [ [solution-1 solution-2] (solve-quadratic -1 time (- distance))
        s-max (Math/ceil (max solution-1 solution-2))
        s-min (Math/floor  (min solution-1 solution-2))]
    (long (dec (- s-max s-min)))))

{:part-1 (time (reduce * 1 (map ways-to-win (parse input))))
 :part-1-fast (time (reduce * 1 (map ways-to-win-analytic (parse input))))
 :part-2 (future (time (ways-to-win (join-times-and-distances (parse input)))))
 :part-2-fast (time (ways-to-win-analytic (join-times-and-distances (parse input))))}
