(ns adventofcode.2025.day04
  (:require
   [adventofcode.utils :as u]
   [clojure.string :as str]))

(def ex "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(defn parse [x]
  (into {}
        (u/grid-seq
         (str/split-lines x))))

(defn removeable [gm]
  (let [paper-positions (filter (comp #{\@} val) gm)]
    (for [[yx v] paper-positions
          :when (< (get (frequencies (map gm (u/grid-neighbours-8 yx))) \@
                        0)
                   4)]
      yx)))

(defn part-1 [x]
  (count (removable (parse x))))

(defn step [gm]
  (if-let [to-rem (seq (removable gm))]
    (reduce dissoc gm (removeable gm))
    (reduced gm)))

(defn iter [f x]
  (if (reduced? x)
    @x
    (recur f (f x))))

(defn part-2 [x]
  (let [gm (parse x)]
    (get
     (merge-with -
                 (frequencies (vals gm))
                 (frequencies (vals (iter step gm))))
     \@)))

{:part-1 (part-1 (user/day-input))
 :part-2 (part-2 (user/day-input))}
