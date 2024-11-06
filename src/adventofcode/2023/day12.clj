(ns adventofcode.2023.day12
  (:require [clojure.string :as str]))


(require '[automat.viz :refer (view)])
(require '[automat.core :as a])
(require '[automat.fsm :as fsm])
(require '[clojure.java.shell :as sh])

(def ex1 "???.### 1,1,3")

(def separator? #{\. \?})
(def damaged? #{\# \?})

(defn groups-fsm [nums]
  (->> (concat [(a/* \.)]
               (->> nums
                    (map (fn [n] (a/or (repeat n \# ))))
                    (interpose (a/+ \.)))
               [(a/* \.)])
       (apply fsm/concat)))


(view (groups-fsm [1 2 3]))

(defn springs-fsm [s]
  (->> s
       (map {\. (a/or \.)
             \# (a/or \#)
             \? (a/or \. \#)})
       (apply fsm/concat)))

(view (groups-fsm [1 1 3]))
(view (springs-fsm "???.###"))

(defn allowed-fsm [[springs groups]]
  (fsm/intersection (-/dd a (springs-fsm springs))
                    (-/dd b (groups-fsm groups))))


(view _a)
(view _b)

(view (allowed-fsm (parse-line ex1)))

(view (allowed-fsm (parse-line "#...# 1,1")))

(defn count-paths [fsm]
  (let [fsm (fsm/->dfa fsm)
        accept? (fsm/accept fsm)
        i (zipmap (fsm/states fsm) (range))
        cache (atom {})
        count-paths (fn c* [state]
            (or (@cache state)
                (let [res (reduce +
                                  (count (filter accept? (vals (fsm/input->state fsm state))))
                                  (map c*
                                       (remove #{state}
                                               (vals (fsm/input->state fsm state)))))]
                  (swap! cache assoc state res)
                  res)))]
    (count-paths (fsm/start fsm))))

(count-paths (allowed-fsm (parse-line ex1)))

(count-paths (allowed-fsm (parse-line "???.???? 2,2")))

(view (allowed-fsm (parse-line "???.???? 2,2")))

(count-paths (allowed-fsm (parse-line (first (str/split-lines (user/day-input))))))


(view (allowed-fsm (unfold  (parse-line (first (str/split-lines (user/day-input)))))))

(-/jfr-start)

(def n (map-indexed (fn [i l]
                      (println i)
                      (time (count-paths (time (allowed-fsm (unfold (parse-line l)))))))
            (str/split-lines (user/day-input))
            ))


(doall n)
(def p2 (future  (reduce + (take 10000 n))))


(future-cancel p2)

(-/jfr-inspect)



(def n (pmap (fn [l]
               (count-paths (allowed-fsm (parse-line l))))
             (str/split-lines (user/day-input))
             ))


(defn valid? [row group]
  (and (<= group (count row))
       (every? damaged? (subvec row 0 group))
       (or (= group (count row))
           (separator? (nth row group)))))

(defn subvec* [row i]
  (if (< i (count row))
    (subvec row i)
    []))

(defn subvec* [row i]
  (if (< i (count row))
    (subvec row i)
    []))

(def solve
  (memoize
   (fn [row groups]
     (if-not (first groups)
       (if (every? separator? row) 1 0)
       (let [space (- (count row) (reduce + groups) (max 0 (dec (count groups))))]
         (if (< space 0)
           0
           (loop [[g & gs :as groups] groups
                  i 0
                  poss 0]
             (if (> i space)
               poss
               (let [poss (+ poss
                             (if (valid? (subvec row i) g)
                               (solve (subvec* row (+ i g 1)) gs)
                               0))]
                 (if (separator? (row i))
                   (recur groups (inc i) poss)
                   poss))))))))))

(defn parse-line [line]
  (let [[row bs] (str/split line #" ")]
    [(vec row)
     (read-string (str "(" bs ")"))]))

(apply solve (parse-line "???? 1"))

(defn unfold [[row groups]]
  [ (vec (apply concat (interpose [\?] (repeat 5 row)))) (apply concat (repeat 5 groups))])


(->> (user/day-input)
     (str/split-lines)
     (map parse-line)
     (map (partial apply  solve))
     (reduce +)
     )


(->> (user/day-input)
     (str/split-lines)
     (map parse-line)
     (map unfold)
     (map (partial apply  solve))
     (reduce +)
     )



(->> (user/day-input)
     (str/split-lines)
     (map parse-line)
     first
     allowed-fsm
     view

     )
