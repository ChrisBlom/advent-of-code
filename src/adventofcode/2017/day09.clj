(ns adventofcode.2017.day09
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [midje.sweet :refer :all]))

(defn score [level xs]
  (->> xs
       (map (partial score (inc level)))
       (apply + level)))

(defn remove-garbage [s]
  (loop [mode 'normal
         res (StringBuilder.)
         [h & t] s
         garbage-count 0]
    (if-not h
      {:part-1 (score 1 (read-string (str res)))
       :part-2 garbage-count}
      (let [ [mode res todo ngarbage-count]
            (case mode
              garbage (case h
                        \> ['normal res t]
                        \! [mode res (rest t)]
                        [mode res t (inc garbage-count)])
              normal (case h
                       \< ['garbage res t]
                       \, [mode res t]
                       (\[ \]) [mode res t] ; avoid collisions with [ and ]
                       \{ [mode (.append res \[) t]
                       \} [mode (.append res \]) t]
                       [mode (.append res h) t]))]
        (recur mode res todo (or ngarbage-count garbage-count))))))

(fact "examples"
  (tabular
   (fact (:part-1 (remove-garbage ?str)) => ?n)

   ?str                            ?n
   "{}"                            1
   "{{{}}}"                        6
   "{{},{}}"                       5
   "{{{},{},{{}}}}"                16
   "{<a>,<a>,<a>,<a>}"             1
   "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
   "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
   "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3))

(remove-garbage (slurp (io/resource "2017/day09.txt")))
