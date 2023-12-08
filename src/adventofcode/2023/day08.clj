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

(def input   (slurp (io/resource "2023/day08.txt")))

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

(defn steps-to-end [{:keys [nodes dirs]} start end-node?]
  (reduce (fn [ [steps node] lr]
            (if (end-node? node)
              (reduced steps)
              [(inc steps)
               (get-in nodes [node lr])]))
          [0 start]
          (apply concat (repeat dirs))))

(defn part-1 [input]
  (steps-to-end (parse input) "AAA" #{"ZZZ"}))

(defn start-node? [s]
  (str/ends-with? s "A"))

(defn end-node? [s]
  (str/ends-with? s "Z"))

(def example-2
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
(defn part-2-naive [input]
  (let [{:keys [dirs nodes]} (parse input)
        repeated-dirs (apply concat (repeat dirs))]
    (reduce (step-2 nodes)
            [0
             (filter start-node? (keys nodes)) ]
            repeated-dirs)))

(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

(defn lcm [a b]
  (* (abs b) (/ (abs a)
                (gcd a b))))

(defn part-2-fast [input]
  (let [{:keys [dirs nodes] :as parsed} (parse input)
        start-nodes (filter start-node? (keys nodes))]
    (->> start-nodes
         (map (fn [start] (steps-to-end parsed start end-node?)))
         (reduce lcm))))

{:part-1 (part-1 input)
 :part-2 (part-2-fast input)}


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
