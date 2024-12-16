(ns adventofcode.2024.day14
  (:require
   [adventofcode.utils :as u]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(defn parse-line [line]
  (let [[px py vx vy] (map parse-long (rest (re-matches #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)" line)))]
    {:pos [px py]
     :vel [vx vy]}))

(defn parse [x]
  (vec (map-indexed (fn [i l] (assoc (parse-line l) :idx i)) (str/split-lines x))))


(parse ex)

(defn robot-step [ w h {:keys [pos vel]}]
  (let [ [x y] pos
        [dx dy] vel]
    {:pos
     [(mod (+ x dx) w)
      (mod (+ y dy) h)]
     :vel vel}))


(defn steps [f init n]
  (if (zero? n)
    init
    (recur f (f init) (dec n))))


(defn in-middle? [w h {:keys [pos]}]
  (let [[x y] pos
        middle-x (/ (dec w) 2)
        middle-y (/ (dec h) 2)]
    (or (= middle-x x)
        (= middle-y y))))

(in-middle? 11 7 {:pos [5 0]} )
(in-middle? 11 7 {:pos [0 3]} )
(in-middle? 11 7 {:pos [0 4]} )

(defn quadrant? [w h {:keys [pos]}]
  (let [[x y] pos
        middle-x (/ (dec w) 2)
        middle-y (/ (dec h) 2)]
    (cond
      (and (> x middle-x) (> y middle-y))
      :bottom-right
      (and (< x middle-x) (> y middle-y))
      :bottom-left
      (and (> x middle-x) (< y middle-y))
      :top-right
      (and (< x middle-x) (< y middle-y))
      :top-left)))


(defn step-robots [w h bots]
  (map (partial robot-step w h) bots))

(defn part-1 [input w h]
  (let [x (parse input)]
    (->> x
         (map (fn [robot] (steps (partial robot-step w h) robot 100)))
         (keep (partial quadrant? w h))
         frequencies
         vals
         (reduce *)
         )))

(assert (= 12 (part-1 ex 11 7)))


(part-1 (user/day-input) 101 103)

(defn pp [w h r]
  (let [pf (frequencies (map :pos r))]
    (doseq [y (range 0 (inc h))]
      (doseq [x (range 0 (inc w))]
        (print (get pf [x y] \space
                    )))
      (println "|"))))


(defn count-middles [w bots]
  (let [middle-x (/ (dec w) 2)]
    (u/counting #(= middle-x (first (:pos %))) bots)))



(binding [*out*
          (clojure.java.io/writer (clojure.java.io/file "w"))]
  (let [cnt (atom 0)]
    (doseq [ [i s] (map list (range)
                        (take 10000 (iterate (partial step-robots 101 103)
                                             (parse (user/day-input))) ))
            :when (> (count-middles 101 s) 33)
            ]
      (swap! cnt inc)
      (println "==" i "==")
      (pp 101 103 s)
      (println " "))
    @cnt
    ))

(defn part-2 [input]
  (let [x (parse input)]))

(assert (= x (part-2 ex)))

{:part-1 (part-1 (user/day-input))
 :part-2 (time (part-2 (user/day-input)))}


(comment



  )
