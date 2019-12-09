(ns adventofcode.2019.day05
  (:require [midje.sweet :refer :all :except any?]
            [adventofcode.2019.intcode :refer [run-intcode]]
            [clojure.java.io :as io]))

(defn parse [input]
  (->> input
       (format "[%s]")
       read-string))

(def input
  (->> "2019/day05.txt"
       io/resource
       slurp
       parse))

(fact

  (:out (run-intcode input [1]))
  =>
  [0 0 0 0 0 0 0 0 0 13933662])

(fact "part 1"
  (last (:out (run-intcode input [1])))
  =>
  13933662)

(fact "part 2"
  (first (:out (run-intcode input [5])))
  => 2369720)
