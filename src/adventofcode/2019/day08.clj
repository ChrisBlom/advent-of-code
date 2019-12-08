(ns adventofcode.2019.day08
  (:require
   [midje.sweet :refer :all :except [any?]]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-digits [s]
  (map #(Character/getNumericValue %) s))

(defn layers [digits w h]
  (->> digits
       (partition (* w h))))

(def input
  (parse-digits (slurp (io/resource "2019/day08.txt"))))

(defn part-1 [digits]
  (let [freqs (->> (layers digits 25 6)
                   (apply min-key #(count (filter #{0} %)))
                   frequencies)]
    (* (freqs 1) (freqs 2))))

(fact
  (part-1 input)
  =>
  2193)

(defn pixel-char [pixel]
  (case pixel
     0 \░
     1 \█
     2 \space))

(defn print-layer [layer w]
  (doseq [line (partition w layer)
          :let [line-str
                (->> line
                     (map pixel-char)
                     (apply str))]]
    (println line-str))
  (flush))


(defn stack-pixels
  [pixel-back pixel-front]
  (if (= pixel-front 2)
    pixel-back
    pixel-front))

(fact
  (stack-pixels 0 1) => 1
  (stack-pixels 1 0) => 0
  (stack-pixels 1 2) => 1
  (stack-pixels 0 2) => 0)

(defn stack-layers [back front]
  (map stack-pixels back front))

(fact
  (->> (layers (parse-digits "0222112222120000") 2 2)
       reverse
       (reduce stack-layers))
  [0 1
   1 0])

(fact
  (stack-layers [1 0 0] [0 0 1])
  =>
  [0 0 1])

(fact
  (stack-layers [1 0 0] [2 2 2])
  =>
  [1 0 0])

(fact "part-2"
  (let [w 25
        h 6]
    (-> (->> (layers input w h)
             (reverse)
             (reduce stack-layers))
        (print-layer w)
        with-out-str))
  =>
  (str/join \newline
   ["█░░░█████░█░░█░████░████░"
    "█░░░██░░░░█░░█░█░░░░█░░░░"
    "░█░█░███░░████░███░░███░░"
    "░░█░░█░░░░█░░█░█░░░░█░░░░"
    "░░█░░█░░░░█░░█░█░░░░█░░░░"
    "░░█░░████░█░░█░████░█░░░░"
    ""]))
