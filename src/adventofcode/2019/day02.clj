(ns adventofcode.2019.day02
  (:require [midje.sweet :refer :all :except any?]
            [clojure.java.io :as io]))

(defn parse [input]
  (->> input
      (format "[%s]")
      read-string))

(def input
  (->> "2019/day02.txt"
       io/resource
       slurp
       parse))

(defn run-intcode
  ([intcode noun verb]
   (-> input
       (assoc 1 noun
              2 verb)
       run-intcode))
  ([intcode]
   (loop [ instruction-pointer 0
          buffer intcode]
     #_(println buffer instruction-pointer)
     (let [[opcode a b c] (subvec buffer instruction-pointer)]
       (case opcode
         1 (recur (+ 4 instruction-pointer)
                  (assoc buffer c (+ (get buffer a) (get buffer b))))
         2 (recur (+ 4 instruction-pointer)
                  (assoc buffer c (* (get buffer a) (get buffer b))))
         99 buffer)))))

(fact
  (run-intcode [1 0 0 0 99])
  => [2 0 0 0 99]
  (run-intcode [2,3,0,3,99])
  => [2,3,0,6,99]
  (run-intcode [2 4 4 5 99 0])
  => [2,4,4,5,99,9801 ]
  (run-intcode [1,1,1,4,99,5,6,0,99])
  => [30,1,1,4,2,5,6,0,99]
  )

{:part-1 (-> (run-intcode input 12 2)
             first)
 :part-2 (-> (for [noun (range 0 (inc 99))
                   verb (range 0 (inc 99))
                   :let [output (first (run-intcode input noun verb))]
                   :when (= output 19690720)]
               (+ (* 100 noun) verb))
             first)}
