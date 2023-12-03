(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[clojure.test :as t])


;; matches a sequence of digits or a sequence of (not (digits or dots))
(def number-or-symbol-pattern  #"(\d+)|([^\.\d]+)" )

(defn parse-line [y line]
  (let [m (re-matcher number-or-symbol-pattern line)]
    (loop [results []]
      (if-let [ [match number symbol] (re-find m)]
        (recur (conj results
                     (cond-> {:coords
                              (mapv (fn [x] {:y y :x x}) (range (.start m) (.end m)))
                              }
                       number (assoc :number (parse-long number))
                       symbol (assoc :symbol symbol))))
        results))))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map-indexed parse-line)
       (apply concat)))

;; annoying edge cases
(assert (= (parse-line 0 "...*123...")
           [{:coords [{:y 0, :x 3}], :symbol "*"} {:coords [{:y 0, :x 4} {:y 0, :x 5} {:y 0, :x 6}], :number 123}]
           ))
(assert (= (parse-line 0 "...456#")
           [{:coords [{:y 0, :x 3} {:y 0, :x 4} {:y 0, :x 5}], :number 456} {:coords [{:y 0, :x 6}], :symbol "#"}]))


(defn neighbourhood [{:keys [y x]}]
  (for [dy [-1 0 1]
        dx [-1 0 1]
        :when (not (= dx dy 0))]
    {:y (+ y dy)
     :x (+ x dx)}))

(defn part-1 [input]
  (let [parsed (parse-input input)

        symbol-coordinates
        (->> parsed
             (filter :symbol)
             (mapcat :coords))

        adjacent-to-symbol-coordinates
        (->> symbol-coordinates
             (mapcat neighbourhood)
             (into #{}))

        parts-adjacent-to-symbol
        (->> parsed
             (filter :number)
             (filter (fn [{:keys [coords]}]
                       (some adjacent-to-symbol-coordinates coords))))]

    (->> parts-adjacent-to-symbol
         (map :number)
         (apply +))))

(defn part-2 [input]
  (let [parsed (parse-input input)

        coordinate->part
        (->> parsed
             (filter :number)
             (mapcat (fn [part] (map (fn [coord] [coord part]) (:coords part))))
             (into {}))

        gear? (comp #{"*"} :symbol)

        gear-ratios (->> parsed
                         (filter gear?)
                         (keep (fn [{:keys [coords]}]
                                 (let [gear-coord (first coords) ; gears always just 1 coordinate
                                       adjacent-parts (->> gear-coord
                                                           neighbourhood
                                                           (keep coordinate->part)
                                                           distinct)]
                                   (when (= 2 (count adjacent-parts))
                                     (apply * (map :number adjacent-parts)))))))]
    (reduce + 0 gear-ratios)))


(def example
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")


(assert (= 4361 (part-1 example)))
(assert (= 467835 (part-2 example)))

(let [input (slurp (io/resource "2023/day03.txt"))]
  (doto {:part-1 (part-1 input)
         :part-2 (part-2 input)}
    println))
