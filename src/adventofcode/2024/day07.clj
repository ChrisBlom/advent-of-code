(ns adventofcode.2024.day07
  (:require
   [adventofcode.utils :as u]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn parse [x]
  (->>
   (str/split-lines x)
   (mapv (fn [x]
          (read-string (format "[%s]" (str/replace x ":" "")))))))

(parse ex)

(defn indicator [x] (if x 1 0))

(defn possible-solutions-1 [ target acc todo]
  (cond
    (empty? todo)
    (indicator (= target acc))

    :else
    (let [ [h & t] todo]
      (+ (possible-solutions-1 target (+ h acc) t)
         (possible-solutions-1 target (* h acc) t)))))

(defn part-1 [input]
  (let [lines (parse input)]
    (reduce  + 0
     (keep (fn [ [target & [ h & t]]]
             (when (pos? (possible-solutions-1 target h t))
               target))
           lines))))

(part-1 ex)

(defn decimals [x]
  (int (math/pow 10 (inc (math/floor (math/log10 x))))))

(defn || [x y]
  (+
   (* x (decimals y))
   y))

(assert (= 123456)(|| 123 456))
(assert (= 1230)(|| 123 0))
(assert (= 1231)(|| 123 1))

(defn calibrated-2? [ target acc todo]
  (cond
    (empty? todo)
    (= target acc)

    ;; all operations will make acc larger or keep acc the same (+ 0, * 1)
    ;; so we can prune branches where acc > target
    (> acc target)
    false

    :else
    (let [ [h & t] todo]
      (or (calibrated-2? target (+ acc h) t)
          (calibrated-2? target (* acc h) t)
          (calibrated-2? target (|| acc h) t)))))

(defn digits [x]
  (if (zero? x)
    1
    (int (inc  (math/floor (math/log10 x))))))

(assert (= (digits 1) 1))
(assert (= (digits 0) 1))
(assert (= (digits 10) 2))
(assert (= (digits 99) 2))
(assert (= (digits 100) 3))

(defn remove-suffix [all suffix]
  (let [a   (/ (- all suffix) (int (Math/pow 10 (digits suffix))))]
    (if (pos-int? a)
      a)))

(assert (= (remove-suffix 123 23) 1))
(assert (= (remove-suffix 123 123)) nil)

(defn calibrated-2-fast? [  acc todo]
  (cond
    (nil? acc)
    false

    (not (integer? acc))
    false

    :else
    (let [h (peek todo)
          t (pop todo)]
      (if (empty? t)
        (= h acc)
        (or
         (calibrated-2-fast?  (/ acc h) t)
         (calibrated-2-fast?  (remove-suffix acc h) t )
         (calibrated-2-fast?  (- acc h) t )
         )))))

(assert (calibrated-2-fast? 156 [15 6]))
(assert (calibrated-2-fast? 7290 [6 8 6 15]))
(assert (calibrated-2-fast? 156 [15 6]))
(assert (calibrated-2-fast? 192 [17 8 14]))

(defn part-2 [input]
  (let [lines (parse input)]
    (->> lines
         (map (fn [ [target & [ h & t :as nums]]]
                (if #_(calibrated-2? target h t)
                    (calibrated-2-fast? target (vec  nums))
                    target)))
         (filter identity)
         (reduce  + 0))))

(assert (* (|| (* 6 8) 6)  15) 7290)
(assert (= (part-2 ex) 11387))

(let [i (user/day-input)]
  {:part-1 (part-1 (user/day-input))
   :part-2 (time (part-2 i ))})
