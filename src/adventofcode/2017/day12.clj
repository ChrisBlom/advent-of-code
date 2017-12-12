(ns adventofcode.2017.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [midje.sweet :refer :all]))

(def input (str/trim (slurp (io/resource "2017/day12.txt"))))

(def example
  "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5")

(defn parse [input]
  (->> input
       str/split-lines
       (mapv (comp read-string (partial format "[%s]")))))

(defn build-graph [input]
  (let [pairs (for [[src _ & trgs] (parse input)
                    trg trgs]
                [src trg])]
    (reduce (fn [acc [src trg]]
              (-> acc
                  (update src (fnil conj #{}) trg)
                  (update trg (fnil conj #{}) src)))
            {}
            pairs)))

(defn group [graph start]
  (loop [visited (sorted-set)
         [h & t :as todo] [start]]
    (if (empty? todo)
      visited
      (recur (conj visited h)
             (concat (remove visited (graph h)) t)))))

(defn remove-nodes [graph nodes]
  (reduce-kv (fn [g src trgs]
               (assoc g src (apply disj trgs nodes)))
             {}
             (apply dissoc graph nodes)))

(defn groups [graph]
  (loop [g graph
         groups []]
    (if (empty? g)
      groups
      (let [arbitrary-node (key (first g))
            group-nodes (group g arbitrary-node)]
        (recur (remove-nodes g group-nodes)
               (conj groups group-nodes))))))

(fact "examples"
  (group (build-graph example) 0)
  =>
  #{0 2 3 4 5 6}

  (groups (build-graph example))
  => [#{0 2 3 4 5 6} #{1}])

{:part-1 (count (group (build-graph input) 0))
 :part-2 (count (groups (build-graph input)))}
