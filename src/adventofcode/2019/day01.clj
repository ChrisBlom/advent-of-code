(ns adventofcode.2019.day01
  (:require [midje.sweet :refer :all]
            [clojure.java.io :as io]))

(def input
  (->> "2019/day01.txt"
       io/resource
       slurp
       (format "[%s]")
       read-string))

(defn fuel-required [mass]
  (-> mass
      (/ 3)
      long
      (- 2)))

(fact "part 1 examples"
  (fuel-required 12) => 2
  (fuel-required 14) => 2
  (fuel-required 1969) => 654
  (fuel-required 100756) => 33583)

(defn fuel-required-incl-fuel [mass]
  (->> (iterate fuel-required mass)
       rest
       (take-while pos?)
       (apply +)))

(fact "part 2 examples"
  (fuel-required-incl-fuel 14) => 2
  (fuel-required-incl-fuel 1969) => 966)

{:part-1 (apply + (map fuel-required input))
 :part-2 (apply + (map fuel-required-incl-fuel input))}
