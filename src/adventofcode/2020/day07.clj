(ns adventofcode.2020.day07
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def example
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(def input (slurp (io/resource "2020/day07.txt")))

(def line1 "dotted black bags contain no other bags.")
(def line2 "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.")


(defn parse-outer [outer]
  (let [ [adjective color] (str/split outer #" ")]
    {:amount 1
     :color (keyword adjective color)}))

(defn parse-contained [contained]
  (let [ [n adjective color] (str/split contained #" ")]
    {:amount (Integer/parseInt n)
     :color (keyword adjective color)}))

(defn parse-line [line]
  (let [ [outer inner] (str/split line #" contain ")
        contained (if (= inner "no other bags.")
                    []
                    (str/split inner #"(, )|\.")
                    )]

    [(:color (parse-outer outer))
     (mapv parse-contained contained)]))

(comment
  (parse-line line1)
  (parse-line line2)
  )

(defn reverse-index [ outer-inners]
  (reduce
   (fn [acc [outer inners]]
     (reduce
      (fn [acc- inner]
        (update acc- (:color inner) (fnil conj #{}) outer))
      acc
      inners))
   {}
   outer-inners))

(def indexed (reverse-index (map parse-line (str/split-lines input))))

;; part 1
{:part-1
 (loop [todo [:shiny/gold]
        visited #{}]
   (if-not (seq todo)
     (count (disj visited :shiny/gold))
     (let [nexts (indexed (first todo))]
       (recur
        (concat (rest todo) (remove visited nexts))
        (conj visited (first todo))
        ))))}

(def example-2
  "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")


(def m (into {} (map parse-line (str/split-lines input))))

(defn contain-count [ amount color]
  (let [contained (m color)
        s (reduce +
                  amount
                  (map (fn [inner]
                         (contain-count (* amount (:amount inner))
                                        (:color inner)))
                       contained))
        ]
;    (println amount color)
 ;   (println "=>" contained)
    s
    ))


(contain-count 1 :dark/green)
(contain-count 1 :dark/violet)
(contain-count 1 :dark/blue)

{:part-2
 (dec (contain-count 1 :shiny/gold))}
