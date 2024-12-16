(ns adventofcode.2024.day11
  (:require
   [adventofcode.utils :as u]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "")

(defn parse [x]
  )

(parse ex)

(defn part-1 [input]
  (let [x (parse input)]
    ))





(comment



  )

(defn digits [x]
  (if (zero? x)
    1
    (int (inc  (math/floor (math/log10 x))))))

(map digits [0 1 9 10 100 1000])


(defn split-num [x]

  (let [d (digits x)]
    (when (even? d)
      (let [factor-half-d (math/pow 10 (/ d 2) )
            dd (/ x factor-half-d )]
        [(int dd)
         (int (- x (* factor-half-d (int dd))))]))))

(split-num 12)
(split-num 1234)
(split-num 123456)

(defn steps [seen i n c]
  (let [xs  (or (if-let [s (seen n)]
                [:already-seen-at s
                 :n n
                 :count c])
              (let [stones (or (when (zero? n) [1])
                               (split-num n)
                               [(* n 2024)])
                    seen- (assoc seen n {:i i :c c})]
                (for [s stones]
                  (steps seen- (inc i) s c))))]
    [n xs]))


(steps {} 0 1 1  )
{:part-1 (part-1 (user/day-input))
 :part-2 (time (part-2 (user/day-input)))}
