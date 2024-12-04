(ns adventofcode.2024.day01)

(def ex "3   4
4   3
2   5
1   3
3   9
3   3")

(defn parse [x]
  (map #(read-string (format "[%s]" %) )
       (clojure.string/split-lines x)))

(defn part-1 [x]
  (let [ [a b ] (map sort (apply map vector (parse x)))]
    (apply + (map abs (map - a b)))))

(part-1 ex)

(defn part-2 [x]
  (let [ [a b ] (map sort (apply map vector (parse x)))
        fb (frequencies b)
        fa (frequencies a)]
    (reduce +
            0
            (for [ [a freq-a] fa] (* a freq-a (get fb a 0))))))

(part-2 ex)

{:part-1 (part-1 (user/day-input))
 :part-2 (part-2 (user/day-input))}
