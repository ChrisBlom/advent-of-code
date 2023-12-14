(ns adventofcode.2023.day10)

(defn map-2d [f-yxv grid]
  (map-indexed (fn [y line]
                 (map-indexed (fn [x v] (f-yxv y x v)) line))
               grid))

(defn parse [s]
  (->> (clojure.string/split-lines s)
       (map-2d (fn [ y x c] [[y x] c]  ))
       (apply concat)
       (into (sorted-map))))

(defn pp [input]
  (run! #(println (apply str (map render % )))
        (clojure.string/split-lines input)))

(pp example)

(def render
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

(defn pipe-path [grid prev-pos pos]
  (let [pipe (grid pos)
        next-pos (first (for [dir (exits pipe)
                              :let [next-pos (mapv + pos (dir-vec dir))
                                    next-pipe (grid next-pos)]
                              :when (and
                                     (not= next-pos prev-pos)
                                     (contains? (exits next-pipe) (opposite dir)))]
                          next-pos))
        next-pipe (grid next-pos)]
    (if (= \S next-pipe)
      [next-pos]
      (cons pos
            (pipe-path grid pos next-pos)))))

(defn init-pos [g]
  (->> g
       (filter (comp #{\S} val))
       (ffirst)))

(def example ".....
.S-7.
.|.|.
.L-J.
.....")

(init-pos (parse example))

(parse example)

(defn part-1 [x]
  (let [g (parse example)]
    (/ 2 (count (pipe-path g nil (init-pos g) )))))

(require '[clojure.math :refer [floor ceil]])


(defn pipe-neighbourhood [[ y x :as  animal-pos]]
  (set [[(ceil y) (ceil x)]
        [(ceil y) (floor x)]
        [(floor y) (ceil x)]
        [(floor y) (floor x)]]))

(defn intersects-pipes [g [y x] dir]
  (case dir
    :u (let [ul (g [(floor y) (floor x)])
             ur (g [(floor y) (ceil x)])]
         (and
          (contains? (exits ul) :r)
          (contains? (exits ur) :l)))
    :d (let [ul (g [(ceil y) (ceil x)])
             ur (g [(ceil y) (floor x)])]
         (and
          (contains? (exits ul) :l)
          (contains? (exits ur) :r)))

    :r (let [ur (g [(floor y) (ceil x)])
             dr (g [(ceil y) (ceil x)])]
         (and
          (contains? (exits ur) :d)
          (contains? (exits dr) :u)))
    :l  (let [ur (g [(floor y) (floor x)])
              dr (g [(ceil y) (floor x)])]
         (and
          (contains? (exits ur) :d)
          (contains? (exits dr) :u)))))


(assert (intersects-pipes (parse
                           "F7
LJ") [1/2 1/2] :l))

(assert (intersects-pipes (parse
                           "F7
LJ") [1/2 1/2] :r))

(assert (intersects-pipes (parse
                           "F7
LJ") [1/2 1/2] :u))

(assert (intersects-pipes (parse
                           "F7
LJ") [1/2 1/2] :d))


(defn reachable [g [ y x :as  animal-pos] ]
  (for [ [d off] dir-vec
        :when (not (intersects-pipes g animal-pos d))
        :let [next-animal-pos (mapv + animal-pos off)]
        ]
    next-animal-pos))

(reachable (parse "F7\nLJ") [1/2 1/2])
(reachable (parse "F7\nLJ") [1/2 -1/2])

(defn flood-fill [g path]
  (let [h (apply max (map first (keys g)))
        w (apply max (map second (keys g)))]
    (loop [front [ [-1/2 -1/2]
                   [-1/2 (- h 1/2)]
                   ]
           visited #{}]
      (if-some [animal-pos (first front)]
        (let [n (reachable g animal-pos)
              n-new (->> n
                         (remove visited)
                         (filter (fn [ [y x]] (and (>= -1 y)
                                                  (< y h)
                                                  (>= -1 x)
                                                  (< x  w)
) )))]
          (recur
           (concat (rest front) n-new)
           (conj visited animal-pos)))
        visited))))


(flood-fill (parse ".F7\n.LJ")
            []
            )
q
(pp (user/day-input))
