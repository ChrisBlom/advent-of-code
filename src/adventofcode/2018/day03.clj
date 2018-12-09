(ns adventofcode.2018.day03
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))


(def ex1 "#123 @ 3,2: 5x4")

(defn parse-line [s]
  (->> (str/split s #"[ |:,x@#]")
       (remove empty?)
       (map #(Long/parseLong %))))


(parse-line ex1)
;; (123 3 2 5 4)

(defn fill-claim [arr [id from-left from-top width height]]
  (reduce
   (fn [acc y]
     (reduce (fn [acc- x]
               (update acc- [x y] (fnil conj #{}) id))
             acc
             (range from-left (+ from-left width))))
   arr
   (range from-top (+ from-top height))))

(def ex (str/split-lines
         "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
"))

(defn fill-claims [input]
  (reduce fill-claim {} (map parse-line input)))

(defn print-claims [c]
  (let [x-max (apply max (map first (keys c)))
        y-max (apply max (map second (keys c)))]
    (dotimes [y y-max]
      (dotimes [x x-max]
        (let [ids (get c [x y])
              n (count ids)]
          (cond
            (= n 0) (print ".")
            ;(= n 1) (print (first ids))
            :else (print n))))
      (println))))

(print-claims (fill-claims ex))

(defn count-conflicts [input]
  (->> (fill-claims input)
       (filter (fn [[k v]] (> (count v) 1)))
       count))

(defn get-non-conflicting-id [input]
  (let [cells->ids (fill-claims input)
        ;; build reverse index
        id->cells (reduce (fn [acc [ [x y] ids]]
                            (reduce (fn [acc- id]
                                      (update acc- id (fnil conj #{}) [x y]))
                                    acc
                                    ids))
                          {}
                          cells->ids)
        ids (keys id->cells)]
    (some
     (fn [id]
       (let [cells (get id->cells id)
             cell-unconflicted? (fn [cell] (= 1 (count (get cells->ids cell))))]
         (when (every? cell-unconflicted? cells)
           id)))
     ids)))

(count-conflicts ex)

(def input
  (line-seq (io/reader (io/resource "2018/day03.txt"))))

{:part-1 (count-conflicts input)
 :part-2 (get-non-conflicting-id input)}
