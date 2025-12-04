(ns adventofcode.2025.day03
  (:require [clojure.string :as str]))

(def ex "987654321111111
811111111111119
234234234234278
818181911112111
")

(defn parse-line [x]
  (mapv
   (comp parse-long str)
   x))

(defn parse [x]
  (mapv parse-line (str/split-lines x)))


(parse ex)

(defn max-elems [line]
  (let [idx-line (map list (range) line)

        highest
        (first (sort-by (comp - second) (butlast idx-line)))

        highest-2
        (first (sort-by (comp - second) (drop (inc (first highest)) idx-line )))
        ]

    (+ (* 10 (second highest))
       (second highest-2)
       )

    #_    (parse-long (str/join (map second (sort-by first (take 2 (sort-by (comp - second) idx-line)))))))
  )

(drop 0 [1])

(max-elems [1 1 2 9])

(max-elems [9 1 9])

(max-elems [0 0 0 5 8 0 0 0 5 9])

(max-elems [0 0 0 9 9 2])

(max-elems [0 0 0 2 9])

(max-elems [1 2 3 ])


(assert (= (map max-elems (parse ex))
           '(98 89 78 92)
           ))

(reduce + (map max-elems (parse (user/day-input))))
17092

(take 10 (parse (user/day-input)))

*


(defn part-1 [ex]
  (reduce + (filter invalid? (mapcat expand-range (parse ex)))))

(defn part-2 [ex]
  (reduce + (filter invalid-2? (mapcat expand-range (parse ex)))))

(defn invalid-2? [x]
  (re-matches
   #"(\d{1,})\1{1,}"
   (str x)))

{:part-1
 (part-1 (user/day-input))
 :part-2
 (part-2 (user/day-input))}

(max-elems [2 2 3 5 3 2 4 2 2 2 2 3 2 2 4 4 3 2 2 4 2 2 3 1 2 2 3 4 2 5 1 3 3 3 3 4 3 4 2 5 2 4 3 3 6 3 4 4 3 1 5 2 2 4 4 1 1 1 1 2 2 6 3 2 3 3 6 2 4 2 2 2 5 7 4 5 4 3 3 4 5 2 4 5 2 4 5 1 3 3 2 4 4 5 5 4 6 4 4 3])



(defn max-elems-2 [idx->elems
                   selected
                   n]

  (if (= n 1)
    (first (sort-by (comp - val) idx->elems))))




(max-elems-2 (zipmap (range) [9 9 2])
             []
             1)
