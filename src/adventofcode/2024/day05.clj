(ns adventofcode.2024.day05
  (:require
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn parse [x]
  (let [[ rules [_ & numbers]] (split-with (complement  #{""}) (str/split-lines x))]
    {:ordering (into #{} (map #(mapv parse-long (str/split % #"\|")) rules))
     :updates (mapv #(mapv parse-long (str/split % #",")) numbers)}))

(defn middle [xs]
  {:pre [(odd? (count xs))]}
  (nth xs (int (/ (count xs) 2))))

(defn in-correct-order? [ordering update-nums]
  (let [pairs (map vector update-nums (rest update-nums))]
    (every? ordering pairs)))

(defn part-1 [x]
  (let [{:keys [ordering updates]} (parse x)
        mids (map middle (filter (partial in-correct-order? ordering) updates))]
    (reduce + 0 mids)))

(assert (= 143 (part-1 ex)))

(defn part-2 [x]
  (let [{:keys [ordering updates]} (parse x)]
    (->> updates
         (remove (partial in-correct-order? ordering))
         (map (fn [upd]
                (->> upd
                     (sort-by identity (comparator (fn [x y] (ordering [x y]))))
                     middle)))
         (reduce + 0))))

(assert (= 123 (part-2 ex)))

(let [input (user/day-input)]
  (time
   {:part-1 (part-1 input)
    :part-2 (part-2 input)}))

(comment

  (user/graphviz-digraph :neato
                         (doseq [ [ a b] (:ordering (parse ex))]
                           (println a "->" b )))

  (user/graphviz-digraph :dot
                         (println "overlap=scale;")
                         (doseq [ [ a b] (:ordering (parse (user/day-input)))]
                           (println a "->" b )))
  )
