(ns adventofcode.2025.day03
  (:require [clojure.string :as str]))

(def ex "987654321111111
811111111111119
234234234234278
818181911112111
")

(defn parse-line [x]
  (mapv
   (comp parse-long str)
   x))

(defn parse [x]
  (mapv parse-line (str/split-lines x)))

(defn idx-max [xs]
  (ffirst (sort-by (comp - second) (map-indexed (fn [i x] [i x]) xs))))

(idx-max [0 1 2 3])
(idx-max [0 2 2 1])

(defn max-elems-2
  ([elems] (max-elems-2 elems 12))
  ([elems n]
   (max-elems-2 elems n 0 (count elems)))
  ([elems n l r ]
   (if (zero? n)
     nil
     (let [max-i (+ l (idx-max (subvec elems l (- r (dec n)))))]
       (cons (elems max-i)
             (max-elems-2 elems
                          (dec n)
                          (inc max-i)
                          r))))))

(defn to-i [ns]
  (parse-long (str/join ns)))

{:part-1 (reduce + (map (comp to-i (fn [x] (max-elems-2 x 2))) (parse (user/day-input))))
 :part-2 (reduce + (map (comp to-i (fn [x] (max-elems-2 x 12))) (parse (user/day-input))))}
