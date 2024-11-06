(ns adventofcode.2023.day23)

(def ex
  "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#")

(defn parse [s]
  (mapv vec (clojure.string/split-lines s)))

(defn single-path-tile-index [row]
  (->> row
       (keep-indexed (fn [idx tile] (when (= \. tile) idx)))
       first))

(defn target-coord [rows]
  [(dec (count rows))
   (single-path-tile-index (last rows))])

(defn start-coord [rows]
  [0
   (single-path-tile-index (rows 0))])

(target-coord (parse ex))

(def up [-1 0])
(def down [1 0])
(def right [0 1])
(def left [0 -1])
(def dirs [up down left right])

(defn in-map? [m [y x]]
  (and (<= 0 y (dec (count m)))
       (<= 0 x (dec (count (first m))))))

(defn legal-dirs [tile]
  (case tile
      nil dirs
      \# nil
      \. dirs
      \> [right]
      \< [left]
      \^ [up]
      \v [down]))

(defn step [rows {:keys [visited pos len prev-tile]}]
  (let [prev-tile (get-in rows pos)]
    (for [dir (legal-dirs prev-tile)
          :let [npos (mapv + pos dir)]
          :when
          (and
           (in-map? rows npos)
           (not (visited npos))
           (not= \# (get-in rows npos)))]
      {:visited (conj visited pos)
       :pos npos
       :len (inc len)
       :prev-tile prev-tile})))


(defn longest-path [rows]
  (let [target (target-coord rows)
        start (start-coord rows)]
    (loop [states [{:visited #{}
                    :pos start
                    :len 0
                    :prev-tile nil}]
           max-len 0]
      (if-some [s (first states)]
        (if (= (:pos s) target)
          (recur (rest states)
                 (max max-len
                      (:len s)))
          (let [next-states (step rows s)]
            (recur (concat next-states (rest states))
                   max-len)))
        max-len))))


{:part-1 (longest-path (parse (user/day-input)))}


(defn simple-step [rows _ dirs {:keys [len visited pos last-junction]}]
  (for [dir dirs
        :let [npos (mapv + pos dir)]
        :when (and
               (in-map? rows npos)
               (not (visited npos))
               (not= \# (get-in rows npos)))]
    {:pos npos
     :visited (conj visited pos)
     :len (inc len)
     :last-junction last-junction}))

(defn distances-between-junctions [rows & {:keys [start]}]
  (let [start (or start (start-coord rows))
        target (target-coord rows)
]
    (loop [states [{:pos start
                    :len 0
                    :visited #{}
                    :last-junction start}]
           visited #{}
           distances (sorted-map)]
      (if-some [s (first states)]
        (let [{:keys [pos len last-junction]} s]
          (let [next-states (simple-step rows visited dirs s)]
            (if (or (= target pos) ;; is junction
                    (> (count next-states) 1))
              (if (or (get-in distances [pos last-junction])
                      (get-in distances [last-junction pos]))
                (recur (concat (rest states))
                       (conj visited pos)
                     distances)
                (recur (concat (rest states)
                               (map (fn [s] (assoc s
                                                  :len 1
                                                  :last-junction pos))
                                    next-states))
                       (conj visited pos)
                       (-> distances
                           (assoc-in [pos last-junction] (:len s))
                           (assoc-in [last-junction pos] (:len s)))))
              (recur (concat (rest states)
                             next-states)
                     (conj visited pos)
                     distances))))
        distances))))

(distances-between-junctions (p2-shim (parse ex)))
(distances-between-junctions (p2-shim (parse (user/day-input))))

(defn p2-shim [rows]
  (->> rows
       (mapv (fn [row]
               (mapv (fn [t]
                       (if ((set "<>^v") t)
                         \.
                         t))
                     row)))))

(set! *warn-on-reflection* true)

(defn longest-walk [g len start end]
  (let [lw (fn lw ^long [visited len start]
             (cond
               (bit-test visited start)
               -1
               (= start end)
               len
               :else
               (reduce max 0
                       (for [ [trg n] (g start) ]
                         (lw (bit-set visited start)
                             (+ len n)
                             trg)))))]
    (lw 0 len start)))

(defn relabel-coords-as-ints [start end d]
  (let [f (atom 0)
        m (memoize (fn [x] (swap! f inc)))
        s (m start)
        odm (into (sorted-map)
                  (-> d
                      (update-keys m)
                      (update-vals (fn [x] (into (sorted-map)
                                                (update-keys x m))))))]
    [s
     (m end)
     odm]))

(defn part-2 [input ]
  (let [rows (p2-shim (parse input))
        [start end distances] (relabel-coords-as-ints (start-coord rows)
                                                      (target-coord rows)
                                                      (distances-between-junctions rows))]
    (longest-walk
     distances
     0
     start
     end)))

(time
 (part-2 ex))

(time
 (part-2 (user/day-input)))

(defn viz
  ([rows] (viz rows "neato"))
  ([rows cmd ]
   (let [[start end distances] (relabel-coords-as-ints
                                (start-coord rows)
                                (target-coord rows)
                                (distances-between-junctions rows))
         x

         (into #{}(for [ [s t+d] distances
                        [t d] t+d]
                    [(sorted-set s t) d]))]
     (spit "out.dot"
           (with-out-str


             (println "digraph foo { ")

             (println end " [color=red]")

             (doseq [ [a d] x
                     ]
               (println (first a) "->" (second a) "[label="d",dir=both]"))
             (println "}")))
     (assert (zero?  (:exit (clojure.java.shell/sh cmd "-Tsvg" "-O" "out.dot"))))
     (clojure.java.browse/browse-url (str "file://" (.getAbsolutePath (clojure.java.io/file "out.dot.svg")))))))

(comment

  (viz (p2-shim (parse (user/day-input))))

  (viz (p2-shim (parse ex)) "dot")

  )

(defn draw-maze [rows]
  (doseq [ [y row] (map list (range) rows)]
    (doseq [ [x tile] (map list (range) row)]
      (let [ns (when (not= tile \#)
                 (step rows {:visited #{} :pos [y x] :prev-tile nil :len 0}))]
        (cond
          (> (count ns) 2)
          (print \+)
          :else
          (print ({\. \space, \# \█ } tile tile)))))
    (println)))
