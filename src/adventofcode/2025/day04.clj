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
  (into {} (u/grid-seq (str/split-lines x))))

(defn removeable [gm]
  (for [[yx v] gm
        :when (= v \@)
        :when (< (u/counting (fn [n] (= (gm n) \@))
                             (u/grid-neighbours-8 yx))
                 4)]
    yx))

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
    (-
     (u/counting #{\@} (vals gm))
     (u/counting #{\@} (vals (iter step gm))))))


{:part-1 (part-1 (user/day-input))
 :part-2 (time (part-2 (user/day-input)))}
