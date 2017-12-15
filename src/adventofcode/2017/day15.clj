(ns adventofcode.2017.day15)

(set! *unchecked-math* :warn-on-boxed)

(def ^:const factor-a 16807)
(def ^:const factor-b 48271)

(defn multiple-of [^long n ^long m]
  (or (== 1 m)
      (== 0 (rem n m))))

(defn generate-multiple [^long factor ^long prev ^long m]
  (let [n (rem (* prev factor) 2147483647)]
    (if (multiple-of n m)
      n
      (recur factor n m))))

(defn- same-lower-32-bits
  [^long prev-a ^long prev-b]
  (== (bit-and prev-a 0xffff)
      (bit-and prev-b 0xffff)))

(defn solution [[multiple-a seed-a]
                [multiple-b seed-b] limit]
  (let [limit (long limit)]
    (loop [prev-a seed-a
           prev-b seed-b
           same-count 0
           i (long 0)]
      (if (< i limit)
        (recur (generate-multiple factor-a prev-a multiple-a)
               (generate-multiple factor-b prev-b multiple-b)
               (if (same-lower-32-bits prev-a prev-b)
                 (inc same-count)
                 same-count)
               (inc i))
        same-count))))
(time
    {:part-1 (solution [1 883]
                       [1 879] (long 40e6))
     :part-2 (solution [4 883]
                       [8 879] (long 5e6))})
