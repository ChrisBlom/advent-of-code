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

(def horizontal #{left right})
(def vertical #{up down})
(def all-dirs (sorted-set up right down left))

(defn move [pos dir] (mapv + pos dir))

(def turn-dirs
  {left vertical
   right vertical
   down horizontal
   up horizontal
   nil all-dirs})

(defn allowed-dirs1 [prev-dir steps-in-prev-dir]
  (cond-> (turn-dirs prev-dir)
    (and prev-dir (< steps-in-prev-dir 3)) (conj prev-dir)))

(defn allowed-dirs2 [prev-dir steps-in-prev-dir]
  (cond
    (not prev-dir)
    all-dirs ;; only on initial step
    (< steps-in-prev-dir 4)
    #{prev-dir}
    (< steps-in-prev-dir 10)
    (conj (turn-dirs prev-dir) prev-dir)

    :else
    (turn-dirs prev-dir)))

(allowed-dirs1 nil 1)
(allowed-dirs1 up 1)
(allowed-dirs1 up 2)
(allowed-dirs1 up 3)

(defn target-pos [g]
  [(dec (count g))
   (dec (count (g 0)))])


(def i (atom 0))

(defn pp [g path]
  (let [in-path? (set path)]
    (doseq [y (range (count g))]
      (->> (for [x (range (count (get g y)))]
             (if (in-path? [y x])
               \~
               (get-in g [y x])))
           (apply str)
           println))))

(defn update!
  [m k f]
  (assoc! m k (f (get m k))))


(defrecord State [pos prev-dir steps-in-prev-dir total-loss path visited])

(defn state-id [^State s]
  [(.-pos s) (.-prev-dir s) (.-steps-in-prev-dir s)])

;; basically Dijkstra:
(defn min-heat-loss [allowed-dirs is-part-2 g]
  (let [nid (volatile! 0)
        fresh-id (fn [] (vswap! nid inc))
        target (target-pos g)
        init (map->State {:pos [0 0]
                          :prev-dir nil
                          :steps-in-prev-dir 0
                          :total-loss 0
                          :path [ ]
                          :visited false})]
    (loop [todo (pm/priority-map-keyfn :total-loss (fresh-id) init)
           seen-states (transient {})
           c 0]
      (if-some [ [_ state] (peek todo)] ; entry with lowest :total-loss
        (let [id (state-id state)
              {:keys [pos prev-dir steps-in-prev-dir total-loss path]} state]
          (cond

            (> c 10000000)
            (throw (ex-info "max iterations reached" {:visited-states (count seen-states)} ))

            (if is-part-2
              (and (>= steps-in-prev-dir 4)
                     (= target (:pos state)))
                (= target (:pos state)))
            state

            (let [found (get seen-states id)]
              (or
               ;; already explored this state
               (:visited found)
               ;; there is already a shorter path to this position
               (> (:total-loss state) (:total-loss found Long/MAX_VALUE))))
            (recur (pop todo)
                   seen-states
                   (inc c))

            :else
            (let [succ (for [d (allowed-dirs prev-dir steps-in-prev-dir)
                             :let [npos (move pos d)
                                   loss (get-in g npos)
                                   nsteps-in-prev-dir (if (= d prev-dir) (inc steps-in-prev-dir) 1)]
                             :when loss ; to check grid bounds
                             :let [ntotal-loss (+ total-loss loss)]
                             ;; skip states that are not better that the best known result
                             :when (<= ntotal-loss
                                       (:total-loss (get seen-states [npos d]) Long/MAX_VALUE))]
                         (->State
                          npos
                          d
                          nsteps-in-prev-dir
                          ntotal-loss
                          nil; (conj path npos)
                          false
                          ))]
              (recur (-> todo
                         pop
                         (into (map vector (repeatedly fresh-id) succ)))
                     (update! seen-states id
                              (fn [lowest]
                                (assoc (if (< (:total-loss state)
                                              (:total-loss lowest Long/MAX_VALUE))
                                         state
                                         lowest)
                                       :visited true)))
                     (inc c)))))
        {:err seen-states}))))


(defn part-1 [input]
  (:total-loss (min-heat-loss allowed-dirs1 false
                              (parse input))))

(defn part-2 [input]
  (:total-loss (min-heat-loss allowed-dirs2 true
                              (parse input))))

(def ex2 "111111111111
999999999991
999999999991
999999999991
999999999991")

(assert (= 71 (part-2 ex2)))
(assert (= 94 (part-2 ex)))

{:part-1 (time (part-1 (user/day-input)))
 :part-2 (part-2 (user/day-input))}

(comment
  (part-1 ex)
  (part-1 (user/day-input))

  (time (part-2 (user/day-input)))

  (let [g  (parse ex2)
        r (min-heat-loss allowed-dirs2 true g)]
    (pp g (:path r))
    r)

  (let [g  (parse ex)
        r (min-heat-loss allowed-dirs2 true g)]
    (pp g (:path r))
    r)

  (let [g  (parse (user/d))
        r (min-heat-loss allowed-dirs2 true g)]
    (pp g (:path r))
    r))
