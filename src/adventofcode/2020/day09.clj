(ns adventofcode.2020.day09
    (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def example
  [ 35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(def input (read-string (format "[%s]" (slurp (io/resource "2020/day09.txt")))))

(defn check [n data]
  (let [ [preamble after] (split-at n data)
        sums (set (for [x preamble
                        y preamble
                        :when (not= x y)]
                    (+ x y)))]
    (sums (first after))))

(defn find-invalid [n data]
  (if (check n data)
    (find-invalid n (rest data))
    (first (drop n data))))

(assert (= 127 (find-invalid 5 example)))

{:part-1 (find-invalid 25 input)}

;; TODO part 2
(filter #(= (second %) (find-invalid 25 input))
        (for [a (take-while #(>= (count %) 2) (iterate butlast input))
              b (take-while #(>= (count %) 2) (iterate rest a))
              ]
          [b (reduce + b)]

          ))


(let [target 127]
  (loop [x example
         sum 0
         res '[]
         out '[]]
    (if (and (seq x) (< sum target))
      (let [nsum (+ sum (first x))
            nres (conj res (first x))]
        (recur
         (rest x)
         nsum
         nres
         (conj out [nres nsum])
         ))
      out
      )
    ))

(count
 (filter (comp #(<= % 127) :sum)
         (map (fn [a b]
                {:sum (+ a b)
                 :subseq [a b]})
              example
              (rest example))))
