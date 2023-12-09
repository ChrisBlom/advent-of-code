(ns adventofcode.2022.day08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.set :as set]))

(def example "30373
25512
65332
33549
35390")

(defn parse [input]
  (let [lines (str/split-lines input)]
    {:h (count lines)
     :w (count (first lines))
     :grid
     (->> (map-indexed (fn [y line]
                         (->> line
                              (map-indexed (fn [x n]
                                             [[y x] (parse-long (str n))]))))
                       lines)
          (apply concat)
          (into {}))}))

(defn traverse [{:keys [w h grid]} start n dir]
  (->> (iterate #(mapv + % dir) start)
       (take n)))

(defn in-grid? [ h w [y x]]
  (and (<= 0 x) (< x w)
       (<= 0 y) (< y h)))


(defn getx [m k]
  (if-some [ e (find m k) ]
    (val e)
    (throw (ex-info "not in map" {:k k :m m}))))


(defn select-visible [grid coords]
  (:visible
   (reduce
    (fn [acc coord]
      (let [height (grid coord)]
        (if (> height (:highest acc))
          (-> acc
              (assoc :highest height)
              (update :visible conj coord))
          acc)))
    {:highest -1 :visible #{}}
    coords)))

(defn part-1 [input]
  (let [{:keys [w h grid ] :as g} (parse input)
        traversals
        (concat
         ;; left to right
         (for [y (range 0 h)] (traverse g [y 0] w [0 1]))
         ;; right to left
         (for [y (range 0 h)] (traverse g [y (dec w)] w [0 -1]))
         ;; top to down
         (for [x (range 0 w)] (traverse g [0 x] h [1 0]))
         ;; down to top
         (for [x (range 0 w)] (traverse g [(dec h) x] h [-1 0]))
         )]
    (count (reduce set/union (map (partial select-visible grid) traversals)))))

(part-1 example)
(part-1 (user/load-input))/



(defn part-2 [input]
  (let [{:keys [w h grid] :as g} (parse input)
        traversals (for [ [ty tx] (sort (keys grid))]

                     (for [traversal
                           (concat
                            (map #(vector ty %) (range (inc tx) w))
                            (map #(vector ty %) (map dec (range tx 0 -1)))
                            (map #(vector % tx) (range (inc ty) h))
                            (map #(vector % tx) (map dec (range ty 0 -1))))

                           ]
                       (select-visible grid traversal)

                       ))]
    traversals
    ))




(part-2 "30373
25512
65332
33549
35390
")



(defn select-visible [ grid coords]



  )

/

(def input (user/load-input))
