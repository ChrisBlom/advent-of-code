(ns adventofcode.2017.day06
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.java.io :as io]))

(def name-pattern "([a-z]+)")
(def weight-pattern "\\((\\d+)\\)")

(def pattern
  (re-pattern (str name-pattern
                   " "
                   weight-pattern
                   "("
                   " -> "
                   "(.*)"
                   ")?"
                   )))

(defn parse-line [line]
  (let [[_ name weight _ children] (re-matches pattern line)]
    {:name name
     :weight (read-string weight)
     :children (some-> children (str/split #", "))}))

(comment
  (parse-line "dqzkic (1032) -> inglhm, dgxdydd, hxhhhu")
  (parse-line "dqzkic (1032)"))

(defn parse [strings]
  (->> strings
       clojure.string/split-lines
       (mapv parse-line)))

(def input
  (parse (slurp (io/resource "2017/day07.txt"))))

(def example
  (parse
   "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"))

(defn solutions [input]
  (let [name->node (into {} (map (juxt :name identity) input))

        root (first (set/difference (set (map :name input))
                                    (set (mapcat :children input))))]

    [root
     (-> root
         ;; build tree, decorated with sum & balanced marker
         ((fn expand-node [name]
            (let [{:keys [weight children] :as node} (name->node name)
                  decorated-children (map expand-node children) ; recursively expand children
                  child-sums (map :sum decorated-children)]
              (-> node
                  (assoc :children decorated-children)
                  (assoc :unbalanced (when (seq decorated-children) (apply not= child-sums)))
                  (assoc :sum (apply + weight child-sums))))))
         ;; traverse tree to node that cause imbalance
         ((fn find-corrected-weight [{:keys [children weight unbalanced] :as node}]
            (if-let [unbalanced-child (first (filter :unbalanced children))]
              (find-corrected-weight unbalanced-child) ; child is cause of imbalance, look there
              (let [[heaviest-child lightest-child] (reverse (sort-by :sum children))
                    correction (- (:sum heaviest-child) (:sum lightest-child))]
                (- (:weight heaviest-child) correction))))))]))

(time
    (solutions input))
