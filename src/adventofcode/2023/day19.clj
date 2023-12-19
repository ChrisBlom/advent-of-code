(ns adventofcode.2023.day19
  (:require [clojure.string :as str]))

(def example
  "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
")

(defn parse-flow* [f]
  (let [ [m  a b c d] (re-matches #"([a-z]+)([><])(\d+):([a-zA-Z]+)" f)]
    {:k (keyword a)
     :op (symbol b)
     :n (parse-long c)
     :label (keyword d)}))

(defn parse-flow [l]
  (let [ [_ label flow] (re-matches #"([a-z]+)\{(.*)\}" l)
        parts (str/split flow #",")
        flows (mapv parse-flow* (butlast parts))
        ]
    [(keyword label) (conj flows {:op 'default :label (keyword  (last parts))})]))

(defn parse-rating [s]
  (update-keys (read-string (str/replace s "=" " ")) keyword))

(defn parse [example ]
  (let [ [workflows ratings] (str/split example #"\n\n")]
    {:flows
     (into (sorted-map) (map parse-flow (str/split-lines workflows)))
     :ratings
     (map parse-rating (str/split-lines ratings))}))

(defn run-flow [flows rating]
  (loop [label :in]
    (let [l (loop [todo (flows label)                   ]
              (if-some [ {:keys [k op n label] } (first todo)]
                (case op
                  default label
                  > (if (> (rating k) n)
                      label
                      (recur (rest todo)))
                  < (if (< (rating k) n)
                      label
                      (recur (rest todo)))
                  (throw (ex-info "" {:op op}))
                  )))]
      (case l
        (:R :A) l
        (recur l)))))


(run-flow (:flows (parse example))
          {:x 787,:m 2655,:a 1222,:s 2876}
          )

(run-flow (:flows (parse example))
          {:x 1679,:m 44,:a 2067,:s 496}
          )

(defn part-1 [example]
  (let [{:keys [flows ratings]} (parse example)]
    (->> ratings
         (filter (fn [rating] (= :A (run-flow flows rating)) ))
         (mapcat vals)
         (reduce +)
         )))



(assert (= (part-1 example)
           19114))

(defn split-range [[rmin rmax] op n]
  (let [overlapping (case op
                      default [rmin rmax]
                      > (cond (< n rmin) [rmin rmax]
                              (>= n rmax) nil
                              :else [(inc n) rmax])
                      < (cond (<= n rmin) []
                              (> n rmax) [rmin rmax]
                              :else [rmin (dec n)]))
        remainder (case op
                    default [rmin rmax]
                    > (cond (< n rmin) nil
                            (> n rmax) [rmin rmax]
                            :else
                            [rmin n])
                    < (cond (<= n rmin) [rmin rmax]
                            (> n rmax) nil
                            :else [n rmax]))]
    [overlapping remainder]))

(assert (= (split-range [0 1000] '> 500) [[501 1000] [0 500]]))
(assert (= (split-range [0 500] '> 500) [nil [0 500]] ))
(assert (= (split-range [500 1000] '> 499) [[500 1000] nil]))
(assert (= (split-range [500 1000] '> 500) [[501 1000] [500 500]]))
(assert (= (split-range [0 1000] '< 500) [[0 499] [500 1000]]))
(assert (= (split-range [0 500] '< 500) [ [0 499] [500 500]] ))
(assert (= (split-range [0 400] '< 500) [[0 400] nil]))
(assert (= (split-range [500 1000] '< 1000) [[500 999] [1000 1000]]))

(defn accepting-ranges [flows ranges [{:keys [k op n label] :as n} & tail ]]
  (case op
    default (case label
              :R []
              :A [ ranges]
              (accepting-ranges flows ranges (get flows label)))
    (let [ [matching non-matching] (split-range (get ranges k) op n) ]
      (concat
       (accepting-ranges flows (assoc ranges k matching) (get flows label))
       (accepting-ranges flows (assoc ranges k non-matching) tail)))))

(defn range-combinations [[l u]]
  (inc  (- u l)))

(defn part-2 [example]
  (let [f (assoc  (:flows (parse example))
                  :A [{:op 'default :label :A}]
                  :R [{:op 'default :label :R}]
                  )]
    (->> (accepting-ranges f {:x [1 4000]
                              :m [1 4000]
                              :a [1 4000]
                              :s [1 4000]}
                           (get f :in))
         (map (fn [ranges] (reduce * (map range-combinations (vals ranges)))))
         (apply +))))


(assert (= (part-2 example) 167409079868000))

{:part-1 (part-1 (user/day-input))
 :part-2 (part-2 (user/day-input))}
