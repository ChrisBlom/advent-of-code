(ns adventofcode.2019.day09
  (:require
   [adventofcode.2019.intcode :as intcode]
   [clojure.java.io :as io]
   [midje.sweet :refer :all :except any?]))

(def input (read-string (format "[%s]" (slurp (io/resource "2019/day09.txt")))))

(fact "part 1 examples"
  (let [quine [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]]
    (:out (intcode/run-intcode quine []))
    => quine)

  (first (:out (intcode/run-intcode [1102,34915192,34915192,7,4,7,99,0] [])))
  => #(-> % str count #{16})


  (let [large-num 1125899906842624]
    (first (:out (intcode/run-intcode [104,large-num,99] [])))
    => large-num)

  (:out (intcode/run-intcode [109,12,203,-5,204,-5,99] [1]))
  => [1])

(fact "day 9 part 1"
  (-> input
      (intcode/run-intcode [1])
      :out)
  [2682107844])

(fact "day 9 part 2"
  (time
      (-> input
          (intcode/run-intcode [2])
          :out))
  [34738])
