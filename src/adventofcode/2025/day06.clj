(ns adventofcode.2025.day06
  (:require [clojure.string :as str]))

(def ex "123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +  ")

(defn parse-line [x]
  (read-string (format "[%s]" x)))

(defn parse [x]
  (map parse-line (str/split-lines x)))

(defn part-1 [ex]
  (eval (cons '+ (apply map (comp reverse vector) (parse ex)))))


(defn transpose [vs]
  (apply map vector vs))

(defn part-2 [x]
  (let [lines (str/split-lines x)
        nums (butlast lines)
        ops (read-string (format "[%s]" (last lines)))
        n (apply map (comp parse-long str/trim str) nums)
        g (remove #{'(nil)}(partition-by nil? n))]
    (eval (cons '+  (map cons
                         ops
                         g)))))

(time
 {:part-1 (part-1 (user/day-input))
  :part-2 (part-2 (user/day-input))})
