(ns adventofcode.2023.day17
  (:require
   [clojure.data.priority-map :as pm]))

(def ex
  "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")

(defn parse [s]
  (mapv (fn [line] (mapv (comp parse-long str) line))
        (clojure.string/split-lines s)))

(parse ex)

(def up [-1 0])
(def down [1 0])
(def right [0 1])
(def left [0 -1])

(def opposite
  {left right right left up down down up})

(def desc
  {up :up
   down :down
   left :left
   right :right})

(def horizontal #{left right})
(def vertical #{up down})

(defn move [pos dir] (mapv + pos dir))

(defn scale [ v s]
  [(* s (v 0))
   (* s (v 1))])

(def all-dirs (sorted-set up right down left))

(def allowed-dirs
  {left vertical
   right vertical
   down horizontal
   up horizontal
   nil all-dirs})

(defn next-states [g {:keys [pos prev-dir total-loss] :as s}]
  (apply concat
         (for [d (allowed-dirs prev-dir)]
           (->> (iterate (fn [{:keys [pos total-loss path ] :as s}]
                           (let [npos (move pos d)
                                 loss (get-in g npos)]
                             (when loss
                               (assoc s
                                      :pos npos
                                      :path (conj path npos)
                                      :total-loss (+ total-loss loss)))))
                         (assoc s :prev-dir d))
                (take-while some?)
                (drop 1)
                (take 3)))))

(let [g [[1 2 3 4]
         [5 6 7 8]]]
  (next-states g
               {:pos [0 2]
                :prev-dir right
                :total-loss (+ 1 2 3)}))

(let [g [[1 2 3 4]
         [5 6 7 8]]]
  (next-states g
               {:pos [1 3]
                :prev-dir right
                :total-loss (+ 1 2 3)
                :steps-in-same-dir 3
                }))


(let [g [[1 2 3 4]
         [5 6 7 8]]]
  (next-states g
               {:pos [1 3]
                :prev-dir right
                :total-loss (+ 1 2 3)
                :steps-in-same-dir 3
                }))


(defn coords [g]
  (for [y (range 0 (count g))
        x (range 0 (count (get g y)))]
    [y x]))





(sort-by identity by-min-los [{:total-loss 1} {:total-loss 2} {:total-loss 0}])

(defn dist [a b]
  (reduce + (map (comp abs -)  a b)))

(defn target-pos [g]
  [(dec (count g))
   (dec (count (g 0)))])

(defn min-heat-loss [g]
  (let [target (target-pos g)
        init {:pos [0 0]
              :prev-dir nil
              :total-loss (get-in g [0 0])
              :path [ [0 0] ]}]
    (loop [todo (pm/priority-map-keyfn :total-loss (gensym) init)
           best-so-far {}]
      (if-some [ [k state] (first todo)]
        (if (= target (:pos state))
          (best-so-far target)
          (cond (> (:total-loss state)
                   (get-in best-so-far [(:pos state) :total-loss] Long/MAX_VALUE))
                (recur (dissoc todo k)
                       best-so-far)

                :else
                (let [succ (next-states g state)
                      improvements (filter #(<= (:total-loss %)
                                                (get-in best-so-far [(:pos %) :total-loss] Integer/MAX_VALUE))
                                           succ)
                      best-so-far-updated (reduce (fn [acc {:keys [pos total-loss path]}]
                                                    (assoc acc pos {:total-loss total-loss :path path}))
                                                  best-so-far
                                                  improvements)]
                  (recur (merge (dissoc todo k)
                                (zipmap (repeatedly (count improvements) gensym) improvements))
                         best-so-far-updated))))
        best-so-far))))

(defn br [g]
  [(dec (count g))
   (dec (count (first g)))])

(let [g  (parse ex)]
  (pp g
      (:path (min-heat-loss g))      ))



(defn pp [g path]
  (let [in-path? (set path)]
    (doseq [y (range (count g))]
      (->> (for [x (range (count (get g y)))]
             (if (in-path? [y x])
               \_
               (get-in g [y x])))
           (apply str)
           println))))


(let [g (parse ex)]
  (pp g
      (:path (:))
      ))

(let [g  (parse "111
991")]
  (get (min-heat-loss g) [1 2])
  )

(let [g  (parse "11111
19991
19991
11111")]
  (get (min-heat-loss g) (target-pos g))
)

(let [g  (parse (user/day-input))]
  ((min-heat-loss g)
   (br g)))




(defn A*
 "Finds a path between start and goal inside the graph described by edges
  (a map of edge to distance); estimate is an heuristic for the actual
  distance. Accepts a named option: :monotonic (default to true).
  Returns the path if found or nil."
 [edges estimate start goal & {mono :monotonic :or {mono true}}]
  (let [f (memoize #(estimate % goal)) ; unsure the memoization is worthy
        neighbours (reduce (fn [m [a b]] (assoc m a (conj (m a #{}) b)))
                      {} (keys edges))]
    (loop [q (pm/priority-map start (f start))
           preds {}
           shortest {start 0}
           done #{}]
      (when-let [[x hx] (peek q)]
        (if (= goal x)
          (reverse (take-while identity (iterate preds goal)))
          (let [dx (- hx (f x))
                bn (for [n (remove done (neighbours x))
                         :let [hn (+ dx (edges [x n]) (f n))
                               sn (shortest n Double/POSITIVE_INFINITY)]
                         :when (< hn sn)]
                     [n hn])]
            (recur (into (pop q) bn)
              (into preds (for [[n] bn] [n x]))
              (into shortest bn)
              (if mono (conj done x) done))))))))


(A* nexu)
