(ns adventofcode.2018.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)
(defn opposites [a b]
  (and a b (not= a b)
       (let [a (char a)
             b (char b)]
         (or (= (Character/toLowerCase a) b)
             (= (Character/toUpperCase a) b)))))

(opposites \a \A)
(opposites \A \a)
(opposites \a \a)
(opposites \a \B)

(defn react [i]
  (loop [ [a & as] i
         out []
         just-reacted false]
    (if-not (seq as)
      (conj out a) ; done
      (if (and just-reacted (opposites a (peek out))) ; immediately handle cascading reactions
        (recur as (pop out) true)
        (let [b (first as)]
          (if (opposites a b)
            (recur (rest as) out true)
            ;; advance
            (recur as (conj out a) false)))))))

(defn fixpoint [f x]
  (let [y (f x)]
    (if (= x y)
      x
      (recur f y))))

(def input (str/trim (slurp (io/resource "2018/day05.txt"))))

(defn remove-unit [unit input]
  (let [u-unit (Character/toLowerCase (char unit))]
    (remove (fn [c] (= (Character/toLowerCase (char c)) u-unit)) input)))


(defn best-removal [reacted]
  (let [remaining-units (->> reacted
                             (map #(Character/toLowerCase (char %)))
                             distinct)]
    (apply min (for [r remaining-units]
                 (->> reacted
                      (remove-unit r)
                      (fixpoint react)
                      count)))))

(let [reacted (fixpoint react input)]
  {:part-1 (count reacted)
   :part-2 (best-removal reacted)})
