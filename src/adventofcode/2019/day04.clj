(ns adventofcode.2019.day03
  (:require [midje.sweet :refer :all :except any?]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn conforms- [a b c d e f]
  (and
   (<= a b c d e f)
   (or
    (= a b)
    (= b c)
    (= c d)
    (= d e)
    (= e f))))

(defn conforms [pw]
  (let [digits (map #(Character/getNumericValue %) (str pw))]
    (when (apply conforms- digits)
      digits)))


(fact
  (conforms 112345) => truthy
  (conforms 112341) => falsey

  (conforms 111111) => truthy
  (conforms 223450) => falsey
  (conforms 123789) => falsey)

(defn just-2-adjacent [x]
  (->> x
       (partition-by identity)
       (map count)
       (some #(= % 2 ))))

(fact
  (just-2-adjacent [1 1 2 2 3 3]) => true
  (just-2-adjacent [1 2 3 4 4 4]) => false
  (just-2-adjacent [1 1 1 1 2 2]) => true)

(let [conforms-1 (keep conforms (range 353096 843212))]
  {:part-1 (count conforms-1)
   :part-2 (count (filter just-2-adjacent conforms-1) )})
