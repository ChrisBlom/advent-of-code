(ns adventofcode.2023.day01
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


(def example
  "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
")

(def example-2
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixtee")


(def spelled-digits
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})


(defn extract-digits [line]
  (re-seq #"\d" line))

(def digit-or-spelled-digit (re-pattern (str/join "|" (cons "\\d" (keys spelled-digits)))))

(defn extract-digits-or-spelled-number [line]
  (map #(get spelled-digits % %) (re-seq digit-or-spelled-digit line)))


(defn extract-numbers [input extract-digits]
  (transduce
   (comp
    (map (fn [line]
           (let [digits (extract-digits line)]
             (str (first digits) (last digits)))))
    (map read-string))
   +
   0
   (str/split-lines input)
   ))


(defn part-1 [input] (extract-numbers input extract-digits))
(defn part-2 [input] (extract-numbers input extract-digits-or-spelled-number))

(def input (slurp (io/resource "2023/day01.txt")))



{:part-1 (part-1 input)
 :part-2 (part-2 input)}
