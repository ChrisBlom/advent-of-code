(ns adventofcode.2018.day08
  (:require [clojure.string :as str]))



(def a [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])


[2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2] 0

    [0 3 10 11 12 1 1 0 1 99 2 1 1 2] 1

    [0 3 10 11 12]                    1

(-> [1 2 3 4]
    clojure.zip/vector-zip
    (clojure.zip/down)
    (clojure.zip/right)
    (clojure.zip/node)
    )


(require '[clojure.zip :as z])

(first (drop 2 (iterate inc 0)))

(declare parse-packet-)

(defn parse-packets [ n state]
  (->> (iterate parse-packet- state)
       (drop n)
       first))

(defn parse-packet- [ {:keys [in out]} ]
  (when-let [ [c m & tail] in]
    (cond

      (zero? c)
      {:in (drop m tail)
       :out (conj out {:c c :m m :meta (vec (take m tail)) :value (reduce + 0 (take m tail))})}

      :else
      (let [{:keys [in out]} (parse-packets c {:in tail :out out})
            child-nodes (reverse (take c out))
            meta (take m in)]
        {:in (drop m in)
         :out (conj out {:c c
                         :m m
                         :children child-nodes
                         :meta (vec meta)})}))))

(defn parse-packet2 [ {:keys [in out]} ]
  (when-let [ [c m & tail] in]
    (cond
      (zero? c)
      {:in (drop m tail)
       :out {:c c :m m :meta (vec (take m tail)) :value (reduce + 0 (take m tail))}}

      :else
      (let [{:keys [in out]}   (->> (iterate parse-packet2 state)
                                    (drop n)
                                    first)
            child-nodes (reverse (take c out))
            meta (take m in)]
        {:in (drop m in)
         :out (conj out {:c c
                         :m m
                         :children child-nodes
                         :meta (vec meta)})}))))

(conj (conj '() 1) 2)

(defn parse-packet [xs]
  (first (:out (parse-packet- {:in xs :out (list)}))))

(parse-packet [1 1 0 1 99])
(parse-packet [0 3 10 11 12])
(parse-packet [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])


(parse-packet [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(parse-packet [1 1 1 1 0 1 33 22 11])

(parse-packet [1 1 2 1 0 1 33 0 1 44 22 11])

(defn metadata-sum [in]
  (reduce +(mapcat :meta (:out (parse-packet {:in in :out []})))))




(parse-packet {:in a :out '()})

(assert (= 138 (metadata-sum a)))

(->> (user/day-input)
     str/split-lines
     (map #(read-string (str "(" % ")")))
     (map metadata-sum)
     (reduce +)
     )
