(ns adventofcode.2021.day14
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def example "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(defn parse [input]
  (let [ [start _ & rules] (str/split-lines input)
        parse-rule (fn [line]
                     (let [ [from to] (str/split line #" -> ")]
                       [ (vec from) (first to) ]))]
    {:start start
     :rules (into {} (map parse-rule rules))}))

(defn init [start-string]
  {:elements (frequencies start-string)
   :pairs (frequencies (map vector start-string (rest start-string) ))})

(defn step [rules state]
  (reduce-kv
   (fn [ {:keys [pairs elements] :as acc} [a b :as pair] n]
     (if-let [produced (when (pos? n) (rules pair))]
       {:pairs (-> pairs
                   (update [a produced] (fnil + 0) n)
                   (update [produced b] (fnil + 0) n)
                   (update [a b] (fnil - 0) n))
        :elements (-> elements
                      (update produced (fnil + 0) n))}
       acc))
   state
   (:pairs state)))

(defn simulate [input n]
  (let [{:keys [rules start]} (parse input)]
    (nth (iterate (partial step rules) (init start)) n)))

(assert (= 97 (reduce + (vals (:elements (simulate example 5))))))

(defn solution [input n ]
  (let [counts (vals (:elements (simulate input n)))]
    (-
     (apply max counts)
     (apply min counts))))

(assert (= 1588 (solution example 10)))

(let [input  (slurp (io/resource "2021/day14.txt"))]
  {:part-1
   (time (solution input 10 ))
   :part-2
   (time (solution input 40))})
