(ns adventofcode.2018.day01
  (:require [midje.sweet :refer :all]
            [clojure.java.io :as io]))

(def input
  (->> "2018/day01.txt"
       io/resource
       slurp
       (format "[%s]")
       read-string))

{:part-1 (apply + input)
 :part-2 (reduce (fn [ [sum-before seen] change]
                   (let [sum-after (+ sum-before change)]
                     (if (contains? seen sum-after)
                       (reduced sum-after)
                       [sum-after (conj seen sum-after)])))
                 [ 0 (transient (sorted-set 0))]
                 (cycle input))}
