(ns adventofcode.utils
  (:require [clojure.data.priority-map :as pm]))

(defn update!
  [m k f]
  (assoc! m k (f (get m k))))

(def up [-1 0])
(def down [1 0])
(def right [0 1])
(def left [0 -1])

(def dirs [up down left right])

(defn v+ [a b]
  [(+ (a 0) (b 0))
   (+ (a 1) (b 1))])

(defn grid-neighbours [yx]
  (map #(v+ yx %) dirs))

(defn grid-positions [grid]
  (for [y (range 0 (count grid))
        x (range 0 (count (grid 0)))]
    [y x]))

(defn grid-seq [grid]
  (reduce concat (map-indexed (fn [y line]
                                (map-indexed (fn [x v] [[y x] v]) line))
                              grid)))

(defn grid-map
  "takes a fn with two args [y x] and v
   Applies the function for each [y x] position and value at that position
   of the grid and returns a seq of the results"
  [pos-value-fn grid]
  (map-indexed (fn [y line]
                 (map-indexed (fn [x v] (pos-value-fn [y x] v)) line))
               grid))

(def up [-1 0])
(def down [1 0])
(def right [0 1])
(def left [0 -1])

(defn rot-left
  "rotate 90 degree counter-clockwise"
  [[x y]]
  [ (- y) x])

(defn rot-right
  "rotate 90 degree clockwise"
  [[x y]]
  [ y (- x)])


(assert (= right (rot-right up)))
(assert (= left (rot-left up)))


(defn distinct-by [f coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[x :as xs] seen]
                   (when-let [s (seq xs)]
                     (let [k (f x)]
                       (if (contains? seen k)
                         (recur (rest s) seen)
                         (cons x (step (rest s) (conj seen k)))))))
                 xs seen)))]
    (step coll #{})))

(defn counting [pred coll]
  (let [s (volatile! 0)]
    (doseq [x coll]
      (when (pred x)
        (vswap! s inc)))
    @s))

(comment
  (distinct-by :a [{:a 1 :b 1}
                   {:a 2 :b 1}
                   {:a 3 :b 1}
                   {:a 3 :b 1} ]))

(defn dijkstra [start-state next-states-fn dist-fn target-state? state-key]
  (let [nid (volatile! 0)
        fresh-id (fn [] (vswap! nid inc))]
    (loop [todo (pm/priority-map-keyfn dist-fn (fresh-id) start-state)
           seen-states (transient {})
           c 0]
      (if-some [ [_ state] (peek todo)] ; entry with lowest dist-fn
        (cond
          (> c 10000000)
          (throw (ex-info "max iterations reached" {:visited-states (count seen-states)} ))

          (target-state? state)
          state

          (let [found (contains? seen-states (state-key state))]
            (or
             ;; already explored this state
             (:visited found)
             ;; there is already a shorter path to this position
             (> (dist-fn state)
                (dist-fn found))))

          (recur (pop todo)
                 seen-states
                 (inc c))
          :else
          (let [succ (next-states-fn state)]
            (recur (-> todo
                       pop
                       (into (map vector (repeatedly fresh-id) succ)))
                   (update! seen-states state-key
                            (fn [lowest]
                              (assoc (if (< (dist-fn state)
                                            (dist-fn lowest Long/MAX_VALUE))
                                       state
                                       lowest)
                                     :visited true)))
                   (inc c))))
        {:err seen-states}))))


;; TODO
(defn a-star
  [start-state
   next-states-fn
   target-state?
   dist-fn ; distance so far
   heuristic-fn ; admissible heuristic
   state-key ]
  (let [nid (volatile! 0)
        fresh-id (fn [] (vswap! nid inc))]
    (loop [todo (pm/priority-map-keyfn (fn [s] (+ (dist-fn s) (heuristic-fn s))) (fresh-id) start-state)
           seen-states (transient {})
           c 0]
      (if-some [ [_ state] (peek todo)] ; entry with state-lowest-dist dist-fn
        (cond
          (> c 10000000)
          (throw (ex-info "max iterations reached" {:visited-states (count seen-states)} ))

          (target-state? state)
          state

          (let [found (contains? seen-states (state-key state))]
            (or
             ;; already explored this state
             (:visited found)
             ;; there is already a shorter path to this position
             (> (dist-fn state)
                (dist-fn found))))

          (recur (pop todo)
                 seen-states
                 (inc c))
          :else
          (let [succ (next-states-fn state)]
            (recur (-> todo
                       pop
                       (into (map vector (repeatedly fresh-id) succ)))
                   (update! seen-states state-key
                            (fn [state-lowest-dist]
                              (assoc (if (< (dist-fn state)
                                            (dist-fn state-lowest-dist Long/MAX_VALUE))
                                       state
                                       state-lowest-dist)
                                     :visited true)))

                   (inc c))))
        {:err seen-states}))))
(defn take-while+
  "like `take-while` but includes the first
   item where (pred item) does not return logical true"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) (take-while+ pred (rest s)))
       (list (first s))))))
