(ns adventofcode.2024.day11
  (:require
   [adventofcode.utils :as u]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "0 1 10 99 999")

(defn parse [x]
  (read-string (format "[%s]" x)))

(parse ex)

(defn digits [x]
  (if (zero? x)
    1
    (int (inc  (math/floor (math/log10 x))))))

(assert (= (map digits [0 1 9 10 99 100 1000])
           [1 1 1 2 2 3 4]))

(defn split-num [x]
  (let [d (digits x)]
    (when (even? d)
      (let [factor-half-d (math/pow 10 (/ d 2) )
            dd (/ x factor-half-d )]
        [(int dd)
         (int (- x (* factor-half-d (int dd))))]))))

(defn stone-step [n]
  (or (when (zero? n) [1])
      (split-num n)
      [(* n 2024)]))

(comment
  (stone-step 0)
  (stone-step 1)
  (stone-step 12)
  (stone-step 112)
  (stone-step 1234)
  (stone-step 123456)

)

(defn blink [stone->freq]
  (reduce
   (fn [ acc [s f]]
     (update acc s (fnil + 0) f))
   {}
   (for [ [stone freq] stone->freq
         nstone (stone-step stone)]
     [nstone freq ])))

(defn steps [stones n]
  (nth (iterate blink (frequencies stones)) n))

(defn part-1 [in]
  (reduce + (map val (steps (parse in) 25))))

(defn part-2 [in]
  (reduce + (map val (steps (parse in) 75))))


{:part-1 (part-1 (user/day-input))
 :part-2 (time (part-2 (user/day-input)))}
