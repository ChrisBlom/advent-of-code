(ns adventofcode.2017.day06
  (:require
   [clojure.string :as str]))

(defn parse [strings]
  (->> strings
       clojure.string/split-lines
       (map (comp int read-string))
       (into-array Integer/TYPE)))

(def input
  [4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5])

(def example
  [0, 2, 7, 0])

(defn solution [part bank]
  (let [n (count bank)]
    (loop [bank bank
           seen {}
           iters 0]
      (let [ [i-max blocks] (reduce-kv (fn [[i-max max] i blocks]
                                         (if (> blocks max)
                                           [i blocks]
                                           [i-max max]))
                                       [0 0]
                                       bank)

            distributed (reduce (fn [acc i] (update acc (mod (+ i 1 i-max) n) inc))
                                (assoc bank i-max 0) ; remove all at max pos
                                (range 0 blocks))]

        (let [prev (seen distributed)
              done (when prev (case part
                                 :a (when (= (:n prev) 1) (inc iters))
                                 :b (when (= (:n prev) 2) (- iters (:at prev)))))]
          (or done
              (recur distributed
                     (update seen distributed (fn [{:keys [n]}] {:at iters :n (inc (or n 0))}))
                     (inc iters))))))))

{:part-1 (solution :a input)
 :part-2 (time (solution :b input))}
