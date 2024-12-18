(ns adventofcode.2024.day18
  (:require
      [shams.priority-queue :as pq]
   [adventofcode.utils :as u]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")

(defn parse [x]
  (mapv (fn [line] (read-string (format "[%s]" line))) (str/split-lines x)))

(parse ex)

(defn update! [m k f ]
  (assoc! m k (f (get m k) )))

(defn shortest-path
  [grid init-state target? next-states score-fn key-fn]
  (loop [todo (conj (pq/priority-queue (comp - score-fn)) (init-state grid))
         best-scores (transient {}) ; key -> state
         c 0]
    (if-some [state (peek todo)]  ;; gets entry with lowest value for score-fn in todo
      (let [state-key (key-fn state)
            state-score (score-fn state)
            best-score-for-key (get best-scores state-key)]
        (cond
          (> c 10000000)
          (throw (ex-info "too many iters" {:todo (count todo)}))

          (target? state)
          state

          (and best-score-for-key (<= best-score-for-key state-score))
          (recur (pop todo) best-scores (inc c))

          :else
          (let [succ (next-states grid state)
                succ (filter (fn [s]
                               (< (score-fn s)
                                  (get best-scores (key-fn s) Long/MAX_VALUE) ))
                             succ)]
            (recur (into (pop todo) succ)
                   (assoc! best-scores state-key state-score)
                   (inc c)))))
      {:exhausted-search-space (count best-scores)})))

(defn init-state [g]
  {:pos [0 0]
   :score 0})

(defn next-states [g {:keys [pos score]}]
  (for [d u/dirs
        :let [np (u/v+ pos d)
              at-np (get-in g np)]
        :when at-np
        :when (not= \# at-np)]
    {:pos np
     :score (inc score)}))

(defn target-state-builder [g]
  (let [t [(dec (count g))
           (dec (count (g 0)))]]
    (assert (get-in g t))
    (fn target? [{:keys [pos]}] (= t pos))))

(defn simulate-drops-all [byte-coords]
  (let [h (inc (reduce max (map second byte-coords)))
        w (inc (reduce max (map first byte-coords)))
        empty-grid (vec (repeat h (vec (repeat w \.))))]
    (vec (reductions
          (fn [acc p] (assoc-in acc p \#))
          empty-grid
          byte-coords))))

(defn find-shortest [g]
  (let [target? (target-state-builder g)]
    (shortest-path g
                   init-state
                   target?
                   next-states
                   :score
                   :pos)))

(defn part-1 [in n]
  (let [bytes (parse in)
        g (get (simulate-drops-all bytes) n)]
    (:score (find-shortest g))))

(assert (= 22 (part-1 ex 12)))

(defn part-2 [x]
  (let [bytes-coords (parse x)
        grids (simulate-drops-all bytes-coords)
        ;; binary search
        n (loop [low 0
                 high (count bytes-coords)]
            (let [mid (int (/ (+ low high) 2))]
              (cond (= low mid)
                    low

                    (:exhausted-search-space (find-shortest (grids mid)))
                    (recur low mid)

                    :else
                    (recur mid high))))]
    (str/join "," (first (drop n bytes-coords)))))


(part-2 ex)

{:part-1 (time (part-1 (user/day-input) 1024))
 :part-2 (time (part-2 (user/day-input)))}
