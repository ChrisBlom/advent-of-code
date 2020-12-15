(ns adventofcode.2020.infi)

;; https://aoc.infi.nl/

(defn sum-range [start end]
  (* (- end start)
     (+ start (dec end))
     (/ 2)))

(defn octagon-volume [e]
  (* 4
     (+ (sum-range  (/ e 2)
                    (+ e (/ e 2)))
        (* (/ e 2)
           (+ e (/ e 2))))))

(defn required-edge [e]
  (->> (range)
   (map (fn [edge-length] [edge-length (octagon-volume edge-length)]))
   (filter (fn [[edge-length volume]] (>= volume e)))
   ffirst))

(defn octagon-surface [edge-lenght]
  (* edge-lenght 8))

{:part-1 (required-edge 17477109)
 :part-2
 (->>
  '{"Asia" 4541481661
    "Africa" 1340910581
    "Europe"  747682584
    "South America"  430870926
    "North America"  368945377
    "Oceania"   42717501}
  vals
  (map (comp octagon-surface required-edge))
  (reduce +))}
