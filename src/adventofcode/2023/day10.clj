(ns adventofcode.2023.day10)

(def render
  {\F  \┌
   \L  \└
   \7  \┐
   \J  \┘
   \|  \│
   \-  \─
   \S  \S
   \.  \.})

(def render-bold
  {\F  \┏
   \L  \┗
   \7  \┓
   \J  \┛
   \|  \┃
   \-  \━
   \S  \S
   \.  \.})

(def exits
  {\F #{:d :r}
   \L #{:u :r}
   \7 #{:l :d}
   \J #{:u :l}
   \| #{:u :d}
   \- #{:l :r}
   \S #{:u :l :d :r}})

(def opposite
  {:l :r, :r :l, :u :d, :d :u})

(def dir-vec
  {:l [0 -1]
   :r [0 1]
   :u [-1 0]
   :d [1 0]})

(def example ".....
.S-7.
.|.|.
.L-J.
.....")

(defn map-2d [f-yxv grid]
  (map-indexed (fn [y line]
                 (map-indexed (fn [x v] (f-yxv y x v)) line))
               grid))

(defn parse [s]
  (->> (clojure.string/split-lines s)
       (map-2d (fn [ y x c] [[y x] c]  ))
       (apply concat)
       (into (sorted-map))))

(defn find-start-pos [ g ]
  (ffirst (filter (comp #{\S} val) g)))

(defn start-shape [g start-pos]
  (let [fitting-exits (set
                       (for [ [d dv] dir-vec
                             :let [p (mapv + start-pos dv)]
                             :when (contains? (exits (g p)) (opposite d))]
                         d))]
    (get (clojure.set/map-invert exits) fitting-exits)))

(assert (= [1 1] (find-start-pos
                  (parse "...
.S-
.|."))))

(assert
 (= \F
    (start-shape
     (parse "...
.S-
.|.")

     [ 1 1]
     )))

(defn pp [input]
  (run! #(println (apply str (map render %)))
        (clojure.string/split-lines input)))

(pp example)

(defn pipe-path
  ([grid ]
   (let [start-pos (find-start-pos grid)
         start-shape (start-shape grid start-pos)]
     (loop [pos start-pos
            prev-pos nil
            seen #{}
            path []]
       (if (seen pos)
         path
         (let [pipe (grid pos)
               next-pos (first (for [out-dir (exits pipe)
                                     :let [next-pos (mapv + pos (dir-vec out-dir))
                                           next-pipe (grid next-pos)]
                                     :when (and
                                            (not= next-pos prev-pos)
                                            (contains? (exits next-pipe)
                                                       (opposite out-dir)))]
                                 next-pos))
               next-pipe (grid next-pos)]
           (if-not next-pos
             []
             (recur next-pos
                    pos
                    (conj seen pos)
                    (conj path pos)
                    ))))))))

(defn part-1 [x]
  (-> x
      parse
      pipe-path
      count
      (/ 2)) )

(part-1 example)



(pp (user/day-input))

(pp (user/day-input))


(defn pp [input]
  (let [g (parse input)
        path (set (pipe-path g))]
    (run! (fn [line]
            (println (apply str line))
            )
          (map-2d (fn [y x c]

                    ((if (path [y x])
                       render-bold
                       render) c)
                    )(clojure.string/split-lines input)))))


(pp (user/day-input))


(map dir-vec (exits \-))



(scale [0 0] 3)
(scale [0 1] 3)




(defn ppg [s]
  (println "  ")
  (let [g (if (string? s) (parse s) s)
        y-min (apply min (map first (keys g)))
        y-max (apply max (map first (keys g)))
        x-min (apply min (map second (keys g)))
        x-max (apply max (map second (keys g)))
        path? (set (pipe-path g))
        outer (flood-fill g)]
    (doseq [y (range y-min (inc y-max))]
      (println
       (apply str
              (for [x (range x-min (inc x-max))]
                (cond
                  (path? [y x]) (render-bold (g [y x]))
                  (outer [y x]) "x"
                  :else (render (g [y x])))))))))


(defn extend-grid [g]
  (let [start-pos (find-start-pos g)
        start-shape (start-shape g start-pos)]
    (-> (->> (assoc g start-pos start-shape)
             (mapcat (fn [ [pos c] ] (extend-cell pos c)))
             (into {}))
        (assoc (scale start-pos 3) \S))))


(ppg
 ".......
.......
.S---7.
.|...|.
.L7..|.
..|..|.
..L--J.
.......")

(ppg
 (extend-grid
  (parse
   ".......
.......
.S---7.
.|...|.
.L7..|.
..|..|.
..L--J.
.......")))


(ppg
 (into {}
       (mapcat
        (fn [ [pos c] ] (extend-cell pos c))
        (parse (user/day-input)))))

(ppg
 (into {}
       (mapcat
        (fn [ [pos c] ] (extend-cell pos c))
        (parse "F"))))

(ppg
 (into {}
       (parse (user/day-input))))


(def complex-loop
  "..F7.
.FJ|.
SJ.L7
|F--J
LJ...")

(pp complex-loop)

(ppg complex-loop)
(ppg (extend-grid (parse complex-loop)))

(part-1 complex-loop)

(ppg (extend-grid (parse  complex-loop)))

(ppg (extend-grid (parse (user/day-input))))

(ppg  (parse  complex-loop))


(ppg
 (extend-grid
  (parse "S7
LJ")
  ))

(defn scale [ v s]
  [(* s (v 0))
   (* s (v 1))])

(defn extend-cell [ pos c ]
  (let [pos (scale pos 3)]
    (reduce (fn [g d]
              (assoc g
                     (mapv + pos (dir-vec d))
                     (case d
                       (:u :d) \|
                       (:l :r) \-)))
            (into {}
                  (for [dy [-1 0 1]
                        dx [-1 0 1]
                        :let [np (mapv + pos [dy dx])]]
                    [np (if (= 0 dy dx)
                          c
                          \.)]))
            (exits c))))

(defn shrink-cell [ [y x ] c]
  (when (and (zero? (mod y 3 ))
             (zero? (mod x 3 )))
    [ [(/ y 3) (/ x 3)] c]))


(defn shrink-grid [g]
  (->> g
       (keep (partial apply shrink-cell))
       (into {})))


(defn flood-fill [extended-grid]
  (let [path (set  (pipe-path extended-grid))
        min-x (apply min (map second (keys extended-grid)))
        min-y (apply min (map first (keys extended-grid)))
        max-x (apply max (map second (keys extended-grid)))
        max-y (apply max (map first (keys extended-grid)))
        outer-edge   (concat
                      (for [y (range min-y (inc max-y))
                            x [(dec min-x) (inc max-x)]]
                        [y x] )
                      (for [x (range min-x (inc max-x))
                            y [(dec min-y) (inc max-y)]
                            ]
                        [y x])          ; right edge
                      )
        ]

    (loop [todo outer-edge
           visited #{}]
      (if-some [ pos (first todo)]
        (if (or (path pos)
                (visited pos))
          (recur (rest todo)
                 visited)
          (recur (->>
                  (for [ [d dv]  dir-vec
                        :let [next-pos (mapv + pos dv)
                              next-c (extended-grid next-pos)]
                        :when (and
                               (some? next-c )
                               (not (path next-pos))
)]
                    next-pos)
                  (remove visited)
                  (concat (rest todo)))
                 (conj visited pos)))
        visited))))




(def e2 "..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........")



(def e3 ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")


(ppg (parse e2))
(ppg (extend-grid  (parse e2)))
(ppg (shrink-grid (extend-grid  (parse e2))))


(ppg (extend-grid (parse ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")))


(defn part-2 [s]
  (let [g (parse s)
        e-g (extend-grid g)
        path (pipe-path e-g)
        outer (flood-fill e-g)
        inner
        (apply dissoc
               e-g
               (concat
                path
                outer))]

    (count (shrink-grid inner))
    ))


(def e4 "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
")

(ppg (extend-grid (parse e4)))

(part-2 e4)


(assert (= 1
           (part-2  "F-7
|.|
L-S
")))


(ppg   "F--7.
|..|.
L--S.
.....
")

(assert (= 2
           (part-2  "F--7
|..|
L--S
")))

(part-2 e2)

(part-2 e3)

(part-2 (user/day-input))


{:part-1 (part-1  (user/day-input))
 :part-2

 (-/jfr-record
  (part-2 (user/day-input)))
 }
