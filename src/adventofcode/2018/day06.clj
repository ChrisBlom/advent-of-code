(ns adventofcode.2018.day06
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-line [line]
  (let [[a b] (str/split line #", ")]
    [(Long/parseLong a) (Long/parseLong b)]))

(def ex (map parse-line (str/split-lines "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9
")))

(def input (map parse-line (line-seq (io/reader (io/resource "2018/day06.txt")))))

(defn manhattan-distance [ [^long xa ^long ya] [^long xb ^long yb]]
  (+ (Math/abs (- xa xb))
     (Math/abs (- ya yb))))

(defn nearest-point [centroids [x y :as point]]
  (let [{:keys [min-dist dist->centroid]}
        (reduce
         (fn [{:keys [min-dist] :as acc} c]
           (let [d (manhattan-distance c point)]
             (if (> d min-dist)
               acc
               (-> acc
                   (update-in [:dist->centroid d] (fnil conj []) c)
                   (update :min-dist min d)))))
         {:dist->centoid {}
          :min-dist Long/MAX_VALUE}
         centroids)
        nearest-centroids (get dist->centroid min-dist)]
    (when (= 1 (count nearest-centroids))
      (first nearest-centroids))))

(map (partial nearest-point [[0 0] [10 0]])
     (for [i (range 12)] [i 0]))

(defn extends-to-infinity
  [[[min-x min-y]
    [max-x max-y]
    :as bounding-box]
   [x y]]
  (or (>= x max-x)
      (>= y max-y)
      (<= x min-x)
      (<= y min-y)))

(defn largest-finite-area-size [input]
  (let [min-x (apply min (map first input))
        max-x (apply max (map first input))
        min-y (apply min (map second input))
        max-y (apply max (map second input))
        bounding-box [[min-x min-y] [max-x max-y]]
        centroid->area (volatile! {})]
    (doseq [x (range min-x (inc max-x))
            y (range min-y (inc max-y))]
      (when-let [centroid (nearest-point input [x y])]
        (vswap! centroid->area update centroid (fnil conj #{}) [x y])))
    (apply max (for [[c area] @centroid->area
                     :when (not (some #(extends-to-infinity bounding-box %) area ))]
                 (count area)))))

(largest-finite-area-size ex)



(defn safe-region-size [input max-dist]
  (let [min-x (apply min (map first input))
        max-x (apply max (map first input))
        min-y (apply min (map second input))
        max-y (apply max (map second input))
        bounding-box [[min-x min-y] [max-x max-y]]
        i (volatile! 0)]
    (doseq [x (range min-x (inc max-x))
            y (range min-y (inc max-y))
            :let [p [x y]
                  total (apply + (map #(manhattan-distance p %) input))]
            :when (< total max-dist)]
      (vswap! i inc))
    @i))

(safe-region-size ex 32)

{:part-1 (largest-finite-area-size input)
 :part-2 (safe-region-size input 10000)}
