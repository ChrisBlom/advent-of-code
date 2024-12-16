(ns adventofcode.2024.day16
  (:require
   [shams.priority-queue :as pq]
   [adventofcode.utils :as u :refer [v+ left right up down]]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")

(def ex2 "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################")

(defn parse [x]
  (mapv vec (str/split-lines x)))

(defn start-pos [grid]
  (first (keep (fn [[pos v]]
                 (when (= \S v)
                   pos))
               (u/grid-seq grid))))

(start-pos (parse ex))

(defn target-pos [grid]
  (first (keep (fn [[pos v]]
                 (when (= \E v)
                   pos))
               (u/grid-seq grid))))

(defn next-states [grid {:keys [pos dir score path] :as state}]
  (let [pfw (v+ pos dir)
        dr (u/rot-right dir)
        dl (u/rot-left dir)
        pr (v+ pos dr)
        pl (v+ pos dl)]
    (cond-> []
      (not= \# (get-in grid pr))
      (conj {:pos pr
             :dir dr
             :score (+ score 1001)
             :path (conj path pos)
             })

      (not= \# (get-in grid pl))
      (conj {:pos pl
             :dir dl
             :score (+ score 1001)
            :path (conj path pos)
             })

      (not= \# (get-in grid pfw))
      (conj {:pos pfw
             :dir dir
             :score (inc score)
             :path (conj path pos)
             }))))

(defn update! [m k f ]
  (assoc! m k (f (get m k) )))

(defn shortest-path [grid]
  (let [target (target-pos grid)
        init {:pos (start-pos grid)
              :score 0
              :dir u/right
              :path [ ]
              :visited false}]
    (loop [todo (conj (pq/priority-queue (comp - :score)) init)
           seen-states (transient {})
           c 0]
      (if-some [ state (peek todo)] ; entry with lowest :score
        (let [id (select-keys state [:pos :dir])
              {:keys [pos score dir]} state]
          (cond

            (> c 100000)
            (throw (ex-info "max iterations reached" {:visited-states (count seen-states)} ))

            (= target (:pos state))
            state

            (let [found (get seen-states id)]
              (or
               ;; already explored this state
               (:visited found)
               ;; there is already a shorter path to this position
               (> (:score state) (:score found Long/MAX_VALUE))))
            (recur (pop todo)
                   seen-states
                   (inc c))

            :else
            (let [succ (next-states grid state)]
              (recur (into (pop todo) succ)
                     (update! seen-states id
                              (fn [lowest]
                                (assoc (if (< (:score state)
                                              (:score lowest Long/MAX_VALUE))
                                         state
                                         lowest)
                                       :visited true)))
                     (inc c)))))
        {:err seen-states}))))

(defn part-1 [x]
  (:score (shortest-path (parse x))))

(assert (= 7036 (part-1 ex)))
(assert (= 11048 (part-1 ex2)))

;; variation of shortest-path
;; differences are marked in comments
(defn shortest-paths [grid]
  (let [target (target-pos grid)
        init {:pos (start-pos grid)
              :score 0
              :dir u/right
              :path [ ]
              :visited false}]
    (loop [todo (-> (pq/priority-queue (comp - :score))
                    (conj init))
           seen-states (transient {})
           best-paths []
           best-path-score nil
           c 0]
      (if-some [ state (peek todo)] ; entry with lowest :score
        (let [{:keys [pos score dir]} state
              id [pos dir]]
          (cond

            (= target pos)
            (if (or (nil? best-path-score)
                    (= score best-path-score))

              ;; collection solutions with the optimal score
              (recur (pop todo)
                     seen-states
                     (conj best-paths (update state :path conj pos))
                     score
                     (inc c))

              ;; as solutions are sorted decreasing by score
              ;; we can stop once we get worse solutions
              {:stop best-paths})

            (let [found (get seen-states id)]
              (or
               ;; disabled pruning of visited states as these could
               ;; be reached via different paths and we want all paths
               #_(:visited found)

               ;; there is already a shorter path to this position
               (> score
                  (:score found Long/MAX_VALUE))))
            (recur (pop todo)
                   seen-states
                   best-paths
                   best-path-score
                   (inc c))

            :else
            (let [succ (next-states grid state)]
              (recur (into (pop todo) succ)
                     (update! seen-states id
                              (fn [lowest]
                                ;; replaced < with <= as we want to explore equally good solutions
                                (assoc (if (<= score
                                               (:score lowest Long/MAX_VALUE))
                                         state
                                         lowest)
                                       :visited true)))
                     best-paths
                     best-path-score
                     (inc c)))))

        ;; also stop when we exhausted the search space
        {:stop best-paths}))))

(defn part-2 [x]
  (count (set (mapcat :path (:stop (shortest-paths (parse x)))))))


(assert (= 45 (part-2 ex)))
(assert (= 64 (time (part-2 ex2))))

{:part-1 (part-1 (user/day-input))

 :part-2 (time  (part-2 (user/day-input)))}

(comment



  )
