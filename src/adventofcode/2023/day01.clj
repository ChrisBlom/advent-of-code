
(require
 '[clojure.string :as str]
 '[clojure.java.io :as io])

(def spelled-digits
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})


(defn extract-digits [line]
  (re-seq #"\d" line))

(def digit-or-spelled-digit (re-pattern (str/join "|" (cons "\\d" (keys spelled-digits)))))

(defn extract-digits-or-spelled-number [line]
  (map #(get spelled-digits % %) (re-seq digit-or-spelled-digit line)))


(defn sum-first-last-digits [input extract-digits]
  (transduce
   (map (fn [line]
          (let [digits (extract-digits line)]
            (read-string (str (first digits) (last digits))))))
   + 0
   (str/split-lines input)))


(let [input (slurp "day01.txt")]
  {:part-1 (sum-first-last-digits input extract-digits)
   :part-2 (sum-first-last-digits input extract-digits-or-spelled-number)})
