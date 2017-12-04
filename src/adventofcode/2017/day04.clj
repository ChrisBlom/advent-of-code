(ns adventofcode.2017.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [midje.sweet :refer :all]))

(def input (slurp (io/resource "2017/day04.txt")))

(defn no-duplicate-words? [password]
  (let [xs (clojure.string/split password #" ")]
    (= xs (distinct xs))))

(defn no-duplicate-anagrams? [password]
  (let [xs (map sort (clojure.string/split password #" "))]
    (= xs (distinct xs))))

(defn f4 [part input]
  (let [valid? (case part :a no-duplicate-words? :b no-duplicate-anagrams?)]
    (->> input
         str/split-lines
         (filter valid?)
         count)))

(facts "day04 examples"
  (no-duplicate-words? "aa bb cc dd ee")
  => true
  (no-duplicate-words? "aa bb cc dd aa")
  => false

  (no-duplicate-anagrams? "abcde fghij")
  => true
  (no-duplicate-anagrams? "abcde xyz ecdab")
  => false)

{:part-1 (f4 :a input)
 :part-2 (f4 :b input)}
