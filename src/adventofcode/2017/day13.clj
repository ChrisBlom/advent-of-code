(ns adventofcode.2017.day13
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse [input]
  (->> input
       str/split-lines
       (map #(read-string (format "[%s]" (str/replace % ":" ""))))))

(def example (parse "0: 3
1: 2
4: 4
6: 4"))

(def input (parse (slurp (io/resource "2017/day13.txt"))))

(defn scanner-pos [depth pico]
  (let [period (* (dec depth) 2)
        n (mod pico period)
        m (- period n)]
    (min n m)))

(defn solution-part-1 [input]
  (apply + (for [ [ scanner-layer depth] input
                 :let [pico scanner-layer]
                 :when (zero? (scanner-pos depth pico))]
             (* depth scanner-layer))))

(defn solution-part-2 [input]
  (first (for [delay (range)
               :when (not (some (fn [[scanner-layer depth]]
                                  (let [pico (+ delay scanner-layer)]
                                    (zero? (scanner-pos depth pico))))
                                input))]
           delay)))

(comment

  (defn print-scanners [i in]
    (doseq [ [l depth] in
            :let [v (vec (repeat depth "[ ]"))
                  sp (scanner-pos depth i)]]
      (print l "\t|  ")
      (println (apply str (cond-> (assoc v sp "[#]")
                            (= l i) (assoc ,, 0 (if (= i sp) "(#)" "( )")))))))

  (doseq [i (range 10)]
    (println "-- nano " i "----")
    (print-scanners i example ))

  )


{:part-1 (solution-part-1 input)
 :part-2 (solution-part-2 input)}
