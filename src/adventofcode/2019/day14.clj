(ns adventofcode.2019.day14
  (:require [clojure.string :as str]
            [clojure.java.shell :as sh]
            [clojure.java.io :as io]))

(def trillion 1000000000000)

(defn parse-part [p]
  (let [[amount chemical] (str/split p #" ")]
    [chemical
     (Long/parseLong amount)]))

(defn parse-line [line]
  (let [ parts (map parse-part (str/split line #"(, )|( => )"))]
    {:consumes (vec (butlast parts))
     :produces (last parts)}))

(defn parse [s]
  (->> s
       str/split-lines
       (mapv parse-line)))

(def ex-1
  (parse
   "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL"))

(def ex-2
  (parse "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL"))

(def ex-3
  (parse "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"))

(def input
  (parse (slurp (io/resource "2019/day14.txt"))))

(defn deps [x]
  (->> x
       (reduce (fn [acc {:keys [consumes produces]}]
                 (update-in acc produces (fnil into {}) consumes))
               {})))

(deps ex-1)

(defn map-vals [f m]
  (reduce-kv (fn [acc k v] (assoc acc k (f v))) m m ))

(map-vals {:a 1} inc)

(defn scale [m consumes]
  (->> consumes
       (map-vals (partial * m))))

(scale 2 {:a 2})

(long (Math/ceil (/
                  5 ; requires
                  10 ; produces
                  )))

(defn take-from-stock [stock elems]
  (merge-with - stock elems))

(defn to-produce [stock required]
  (->> (merge-with - required stock)
       (filter (comp pos? val))
       (into {})))

(defn update-stock [stock required]
  (->> (merge-with - stock required)
       (filter (comp pos? val))
       (into {})))


(to-produce {:A 9 :B 3 :C 10}
            {:A 9 :B 2 :C 15}
            )

(update-stock {:A 9 :B 3 :C 10}
              {:A 9 :B 2 :C 15}
            )

(defn m-factor [requires produces]
  (long (Math/ceil (/ requires produces))))

(m-factor 10 1)

(let [produces 10
      requires 9]
  (long (Math/ceil (/ requires produces))))

(let [produces 5
      requires 9]
  (long (Math/ceil (/ requires produces))))


(defn next-negative [stock]
  (->> stock
       (filter (fn [[k v]]
                 (and (neg? v)
                      (not= "ORE" k))))
       first))

(defn update! [m k f v]
  (assoc! m k (f (get m k) v)))

(persistent! (update! (transient {:a 1}) :a + 1))


(defn step [pdeps stock]
  (when-let [[produce-type required-amount] (next-negative stock)]
    (let [ [produces-amount consumes] (first (pdeps produce-type))
          produce-type-in-stock (get stock produce-type)
          need-to-produce (- produce-type-in-stock)
          f (m-factor need-to-produce produces-amount)]
      (-> (reduce-kv (fn [acc c-type c-amount]
                       (update! acc c-type (fnil - 0) (* f c-amount)))
                     (transient stock)
                     consumes)
          (update! produce-type (fnil + 0) (* f produces-amount))
          persistent!))))

(dotimes [_ 10]
  (time
      (dotimes [i 1000]
        (->> {"FUEL" (- i)}
             (iterate (partial step (deps ex-1)))
             (take-while (complement nil?))
             ))))

(-> (->> {"FUEL" -1}
         (iterate (partial step (deps ex-2)))
         (take-while (complement nil?))
         last)
    (get "ORE")
    -
    )

(defn ore-required [input fuel-amount]
  (-> (->> {"FUEL" (- fuel-amount)}
           (iterate (partial step (deps input)))
           (take-while (complement nil?))
           last)
      (get "ORE")
      -))

(time
    (last
     (for [i (iterate inc (long (* 1000 (/ trillion (ore-required input 1000)))))
           :let [ore  (ore-required input i)]
           :while (<= ore trillion)
           ]
       i)))


(ore-required input 1)
(ore-required ex-1 1)
(ore-required ex-2 1)
(ore-required ex-3 1)
