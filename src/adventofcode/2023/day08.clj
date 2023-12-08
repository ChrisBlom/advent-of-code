(ns adventofcode.2023.day07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]))

(def example
  "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(defn parse-node [s]
  (let [[_ from l r] (re-matches #"(.+) = \((.+), (.+)\)" s)]
    [from
     {\L l
      \R r}]))

(assert (= (parse-node "11A = (12B, CC1)")
           ["11A" {\L "12B", \R "CC1"}]))

(assert (= (parse-node "11A = (12B, XXX)")
           ["11A" {\L "12B", \R "XXX"}]))

(defn parse [input]
  (let [ [dirs _ & nodes](->>
                          input
                          str/split-lines)]

    {:dirs dirs
     :nodes (into {} (map parse-node nodes))}))

(parse example)



(defn part-1 [input]
  (let [{:keys [dirs nodes]} (parse input)]
    (reduce (fn [ [i node] lr]
              (if (= node "ZZZ")
                (reduced i)
                [(inc i)
                 (get-in nodes [node lr])]))
            [0
             "AAA"]
     (apply concat (repeat dirs)))))


(defn start-node? [s]
  (str/ends-with? s "A"))

(defn end-node? [s]
  (str/ends-with? s "Z"))


(defn step-2 [nodes]
  (fn [ [i state-nodes] lr]
    (when (mod i  10000) (println i))
    (if (every? end-node? state-nodes)
      (reduced i)
      [(inc i)
       (into #{}
             (map #(get-in nodes [% lr]))
             state-nodes)])))

;; naive solution, too slow
(defn part-2 [input]
  (let [{:keys [dirs nodes]} (parse input)
        repeated-dirs (apply concat (repeat dirs))]
    (reduce (step-2 nodes)
            [0
             (filter start-node? (keys nodes)) ]
            repeated-dirs)))


(parse-node "11A = (11B, 1XXX)")

(keys (:nodes (parse ex2 )))


(def ex2
  "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
")

(part-1 example)

(part-2  ex2)

(def input   (slurp (io/resource "2023/day08.txt")))

(defn export-graph [input]
  (let [ {:keys [nodes]}  (parse input)]
    (spit "out.dot"
          (with-out-str
            (println "digraph foo { ")

            (doseq [from (keys nodes)
                    :let [color (cond (start-node? from) "green"
                                      (end-node? from) "red"
                                      :else "white")]]
              (println from (format " [style=filled, fillcolor = %s ];" color)))
            (doseq [ [from to] nodes]
              (println (str from " -> " (to \L) " [ arrowsize=0.5 , color = blue] "))
              (println (str from " -> " (to \R) " [ arrowsize=0.5 , color = purple] ")))
            (println "}")))
    (apply sh/sh (str/split "neato  -x -Goverlap=false  -Grepulsiveforce=2   -Tsvg -O out.dot" #" +"))
    "out.dot.svg"
    ))


(export-graph input)
