(ns adventofcode.2024.day03
  (:require
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn parse [x]
  (for [ [_ op a b] (re-seq #"(mul)\((\d+),(\d+)\)" x)]
    {:op op
     :a a
     :b b}))

(defn execute [{:keys [op a b]}]
  (case op
    "mul" (*  (read-string a) (read-string b))))

(defn part-1 [x]
  (let [ ops (parse x)]
    (reduce + 0 (map execute ops))))

(def ex-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn parse-2 [x]
  (let [ patterns [#"(mul\(\d+,\d+\))"
                   #"(do\(\))"
                   #"(don't\(\))"
                   ]]
    (map first (re-seq (re-pattern (str/join "|" (map str patterns))) x))))

(parse-2 ex-2)

(defn part-2 [x]
  (first (reduce
          (fn [ [sum enabled] match ]
            (cond
                (str/starts-with? match "mul(")
                (if enabled
                  [(+ sum (part-1 match)) enabled]
                  [sum enabled])
                (str/starts-with? match "don't(")
                [sum false]
                (str/starts-with? match "do(")
                [sum true]))
          [0 true]
          (parse-2 x))))

(assert (= 48 (part-2 ex-2)))

{:part-1 (part-1 (user/day-input))
 :part-2 (part-2 (user/day-input))}
