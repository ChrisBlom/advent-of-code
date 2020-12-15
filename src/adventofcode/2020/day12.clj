(ns adventofcode.2020.day12
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def example
  "F10
N3
F7
R90
F11")

(defn parse [input]
  (letfn [(parse-line [line]
            {:action (first line)
             :value (read-string (subs line 1))})]
    (->>  input
          str/split-lines
          (mapv parse-line))))

(def directions
  {\N [0 1]
   \E  [1 0]
   \S [0 -1]
   \W  [-1 0]})

(defn rotate [dir [dx dy]]
  (case dir
    \L [(- dy) dx]
    \R [dy (- dx)]))

(defn turn [heading action value]
  (let [turns (/ value 90)]
    (-> (iterate (partial rotate action) heading)
        (nth turns))))

(assert
 (=
  (map #(turn (directions \N) \R %) (range 0 360 90))
  (map directions "NESW")))

(assert (= (directions \E) (rotate \R (directions \N))))
(assert (= (directions \W) (rotate \L (directions \N))))

(defn scalar-mul [dir e]
  (mapv * dir [e e]))

(defn move [ {:keys [pos heading] :as state} {:keys [action value]}]
  (case action

    (\N \E \S \W)
    (assoc state :pos (mapv + pos (scalar-mul (directions action) value)))

    (\R \L)
    (assoc state :heading (turn heading action value))

    \F
    (assoc state :pos (mapv + pos (scalar-mul heading value)))))

(defn move-2 [ {:keys [waypoint-pos ship-pos] :as state} {:keys [action value]}]
  (case action

    (\N \E \S \W)
    (assoc state :waypoint-pos (mapv + waypoint-pos (scalar-mul (directions action) value)))

    (\R \L)
    (assoc state :waypoint-pos (turn waypoint-pos action value))

    \F
    (assoc state :ship-pos (mapv + ship-pos (scalar-mul waypoint-pos value)))))

(def input
  (slurp (io/resource "2020/day12.txt")))

(defn manhattan-distance [a b]
  (reduce + (map #(Math/abs %) (map - a b ))))

(let [start-pos [0 0]]
  {:part-1
   (time
       (->> (parse input)
            (reduce move {:heading (directions \E) :pos start-pos})
            :pos
            (manhattan-distance start-pos)))

   :part-2
   (time
       (->> (parse input)
            (reduce move-2 {:waypoint-pos [10 1] :ship-pos start-pos})
            :ship-pos
            (manhattan-distance start-pos)))})
