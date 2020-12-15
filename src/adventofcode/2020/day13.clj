(ns adventofcode.2020.day13
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn parse [s]
  (let [ [timestamp bus-ids] (str/split-lines s)]
    {:timestamp (read-string timestamp)
     :bus-ids (mapv read-string (str/split bus-ids #","))}))

(def example
  (parse "939
  7,13,x,x,59,x,31,19"))

(defn wait-time [timestamp id]
  (- id (mod timestamp id)))

(defn part-1 [ {:keys [timestamp bus-ids]}]
  (->>
   (filter number? bus-ids)
   (map (fn [id] [id (wait-time timestamp id)]))
   (apply min-key second)
   (reduce *)))

(assert (= 295 (part-1 example)))

(def input  (parse (slurp (io/resource "2020/day13.txt"))))

(defn prep [input]
  (sort-by first (keep-indexed (fn [i x] (when (number? x) {:x (- i) :n x})) (:bus-ids input))))

;; based on https://stackoverflow.com/questions/13096491/multiplicative-inverse-of-modulo-m-in-scheme
(defn inverse [x m]
  (loop [x x
         b m
         a 0
         u 1]
    (if (zero? x)
        (if (= b 1) (mod a m)
            (throw (ex-info "must be coprime" {})))
        (let [q (quot b x)]
          (recur (mod b x)
                 x
                 u
                 (- a (* u q)))))))

(defn chinese-remainder [f]
  (let [N (reduce * (map :n f))
        iN (fn [i] (/ N i))]
    (mod (apply +
                (for [{:keys [x n]} f
                      :let [ni (iN n)]]
                  (* x ni (inverse ni n) )))
         N)))

;; based on https://www.youtube.com/watch?v=zIFehsBHB8o
(assert (= 78 (chinese-remainder [{:x 3 :n 5} {:x 1 :n 7} {:x 6 :n 8}])))

(assert (= 3417
         (chinese-remainder (prep (parse "1
17,x,13,19")))))

{:part-1
 (part-1 input)
 :part-2
 (chinese-remainder (prep input))}
