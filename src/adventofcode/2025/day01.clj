(ns adventofcode.2025.day01
  (:require [clojure.string :as str]))




(def ex "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")



(defn parse-line [x]
  (case (first x)
    \R [:R (parse-long (subs x 1))]
    \L [:L (parse-long (subs x 1))]))


(defn parse [s]
  (map parse-line (str/split-lines s)))


(parse ex)



(defn move [pos [dir n]]
  (mod ((case dir
          :R +
          :L - )
        pos n)
       100))

(assert (= 19 (move 11 (parse-line "R8"))))

(defn part-1 [input]
  (->> input
       parse
       (reductions
        move
        50)
       (filter zero?)
       count))



(part-1 (user/day-input))




(defn move-2 [ [zeros pos] [dir n]]
  (assert (int? pos))
  (let [op (case dir :R + :L - )
        res (op pos n)
        zero-x (cond (and (= 0 pos) (< -100 res 100)) 0
                     (= 0 res) 1
                     (pos? res) (quot res 100)
                     (neg? res) (cond-> (quot (* -1 res) 100)
                                  (not= 0 pos) inc))]
    ;; zero's npos
    [
     (+ zeros zero-x)
     (mod res 100)]))


(clojure.test/deftest examples2
  (clojure.test/are [p m e] (= (move-2 p (parse-line m)) e)
    [0  50] "L68" [1 82]
    [0  82] "L30" [0 52]
    [0  52] "R48" [1 0]
    [0  50] "R1000" [10 50]))

(first (reduce move-2 [0 50] (parse ex)))

(first (reduce move-2 [0 50] (parse (user/day-input))))6789
