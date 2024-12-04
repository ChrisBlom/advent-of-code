(ns adventofcode.2023.day22)

(def ex
  "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9")

(defn parse-line [l]
  (mapv (comp vec read-string #(format "[%s]" %)) (clojure.string/split l #"~")))

(def a-z (mapv char (range (int \A) (inc (int \Z)))))

(defn parse [s]
  (->>
   s
   clojure.string/split-lines
   (map parse-line)
   (map-indexed (fn [i x] (conj x (keyword "cube" (str (get a-z i (str i)))))))
   vec
   ))

(defn top-z [ [_ [_ _ hz]]]
  hz)

(defn bottom-z [ [[_ _ lz] _]]
  lz)

(defn label [ [l h id]]
  id)


(parse ex)

(def x 0)
(def y 1)
(def z 2)

(defn sort-stack [cubes]
  (sort-by bottom-z
           cubes))

(defn v-down
  ([ xyz ]
   (update xyz z dec))
  ([ xyz n]
   (update xyz z - n)))

(defn move-down [cube dz]
  (-> cube
      (update 0 v-down dz)
      (update 1 v-down dz)))

(move-down [ [1 1 1] [1 1 1] :a ] 1)
(move-down [ [1 1 10] [1 1 10] :a ] 10)

(defn xy-coords [cube]
  (let [ [[lx ly lz] [hx hy hz]] cube ]
    (for [x (range lx (inc hx))
          y (range ly (inc hy)) ]
      [x y])))

(assert (= (xy-coords [[1 1 1] [1 1 1]])
           [[1 1]]))
(assert (= 100 (count (xy-coords [[1 1 1] [10 10 1]]))))

(defn distinct-by [f coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[x :as xs] seen]
                   (when-let [s (seq xs)]
                     (let [k (f x)]
                       (if (contains? seen x)
                         (recur (rest s) seen)
                         (cons x (step (rest s) (conj seen x)))))))
                 xs seen)))]
    (step coll #{})))

(defn max-all-by [f coll]
  (loop [todo coll
         res []
         highest nil]
    (if-let [h (first todo)]
      (let [k (f h)]
        (cond (or (nil? highest) (> k highest))
              (recur (rest todo)
                     [h]
                     k)
              (= k highest)
              (recur (rest todo)
                     (conj res h)
                     k)
              :else
              (recur (rest todo) res highest)))

      res)))

(defn height-map-lookup [height-map cube]
  (->> (xy-coords cube)
       (map (fn [xy] (get height-map xy {:z 0 :id :floor})))
       (max-all-by :z)))

(time
 (->> (height-map-lookup {[1 1] {:z 1 :id "a"}
                      [2 2] {:z 1 :id "b"}} [ [1 1 2] [ 2 2 2]])
      ))

(defn support-ids [ hm]
  (->> hm
       (map :id)
       (set)))

(defn max-z [ hm]
  (->> hm
       (map :z)
       (reduce max 0)))

(support-ids (height-map-lookup {[1 1] {:z 1 :id "a"}
                              [2 2] {:z 1 :id "b"}} [ [1 1 2] [ 2 2 2]]))

(support-ids (height-map-lookup {[1 1] {:z 1 :id "a"}
                              [2 2] {:z 2 :id "b"}} [ [1 1 20] [ 2 2 20]]))

(max-z  (height-map-lookup {[1 1] {:z 1 :id "a"}
                            [2 2] {:z 2 :id "b"}} [ [1 1 20] [ 2 2 20]]))
;; b

(defn add-to-height-map [ height-map cube]
  (let [ [ [lx ly lz] [hx hy hz] id] cube]
    ;; update height-map with top of cube
    (reduce
     (fn [hm xy]
       (assoc hm xy {:z hz :id  id}))
     height-map
     (xy-coords cube))))

(add-to-height-map {} [ [1 1 1] [1 1 1] :a])
(add-to-height-map {} [ [1 1 1] [1 1 10] :a])

(add-to-height-map {[1 1] {:z 20 :id :x}
                    [20 20] {:z 20 :id :x}}
                   [ [1 1 1] [3 3 10] :a])

(defn move-down-to [ cube new-bottom-z ]
  (let [current-z (bottom-z cube)
        dz (- current-z new-bottom-z)]
    (assert (>= current-z new-bottom-z))
    (move-down cube dz )))

(assert (= (move-down-to [ [1 1 20] [1 1 30] :a] 10)
           [[1 1 10] [1 1 20] :a]))

(defn step [{:keys [height-map supports settled unsettled] :as state}]
  (if (empty? unsettled)
    state
    (let [cube (first unsettled)
          lookup (height-map-lookup height-map cube)
          target-z (inc (max-z lookup))
          cube-dropped (move-down-to cube target-z)]
      {:height-map (add-to-height-map height-map cube-dropped)
       :supports (assoc supports (label cube) (support-ids lookup))
       :settled (conj settled cube)
       :unsettled (next unsettled)})))


(defn stack-cubes [input]
  (->> {:height-map (sorted-map)
        :supports {}
        :settled []
        :unsettled (sort-stack (parse input))}
       (iterate step)
       (drop-while :unsettled)
       first))

(defn invert-deps [ a->deps]
  (reduce-kv
   (fn [acc k vs]
     (reduce (fn [acc- v]
               (update acc- v (fnil conj #{}) k))
             acc
             vs))
   {}
   a->deps))

(defn graph [deps]
  {:-> (invert-deps deps)
   :<- (update-vals deps set)})


(comment

  (def d-in (graph (:supports (stack-cubes (user/day-input)))))

  (def d-ex (graph (:supports (stack-cubes ex)))))

(defn nodes [graph]
  (set (concat (keys (:-> graph))
               (keys (:<- graph)))))

(defn removable? [g n]
  (let [trgs (get-in g [:-> n])]
    (or
     ; no cubes stacked on n
     (empty? trgs)
     ; all cube stacked on n are stacked on another cube
     (every? (fn [t]
               (seq (disj (get-in g [:<- t])
                          n)))
             trgs))))


(comment
  (let [g (graph (:supports (stack-cubes (user/day-input))))]
    (user/graphviz :dot
                   (println "digraph {")
                   (doseq [n (nodes g)]
                     (println (name n) (format "[color=\"%s\"]"
                                               (if (removable? g n)
                                                 "green"
                                                 "red"))))
                   (doseq [ [k vs]  (:-> g)
                           v vs
                           ]
                     (println (name k) "->" (name v))
                     )
                   (println "}"))))

(removable?
 (graph '{A #{B C}
          C #{E D}
          B #{E D}})
 'C
 )

(removable?
 (graph (invert-deps '{A #{B C}
                       C #{E D}
                       B #{E D}}))
 'A
 )

(removable?
 (graph (invert-deps '{A #{B C}
                       C #{E D}
                       B #{E D}
                       D #{F}
                       }))
 'F
 )

(removable?
 (graph (invert-deps '{:floor #{A}
                       A #{B C}
                       C #{E D}
                       B #{E D}}))
 :floor
 )

(defn count-removable [g n]
  (count (filter #(removable? g %) (nodes g))))

(defn part1 [input]
  (count-removable (graph (:supports (stack-cubes input))) :floor))

(part1 (user/day-input))
; ->477


(defn remove-node [graph n]
  (let [ts (get-in graph [:-> n])
        fw (:-> graph)
        bw (:<- graph)
        ]
    {:->
     (update-vals (dissoc fw n)
                  (fn [vs] (disj vs n )))
     :<-      (update-vals (dissoc bw n)
                  (fn [vs] (disj vs n )))
     }))

(defn descendants [graph n]
  (loop [ [h & t] (get-in graph [:-> n])
         visited #{}]
    (if-not h
      visited
      (let [desc (when-not (visited h)
                   (get-in graph [:-> h]))]
        (recur (concat desc t)
               (conj visited h))))))

(defn supported?
  "checks if there is a path to :floor"
  [graph n]
  (loop [ [h & t] [n]
         visited #{}]
    (cond
      (nil? h)
      false

      (= :floor h)
      true

      :else
      (let [desc (when-not (visited h)
                   (get-in graph [:<- h]))]
        (recur (concat desc t)
               (conj visited h))))))

;; TODO could optimize by breaking of search in descendants when
;; encountering a node with a path to :floor,
;; but fast enough now
(defn part-2 [input ]
  (let [g (graph (:supports (stack-cubes input)))
        supporting-nodes (remove #(removable? g %)
                                 (disj (nodes g) :floor))]
    (reduce + 0
            (for [n supporting-nodes]
              (let [d (descendants g n)
                    will-fall (remove (partial supported? (remove-node g n))
                                      d
                                      )]
                (count will-fall))))))

(time
 (part-2 (user/day-input)))

(defn vg [g]
  (user/graphviz :dot
                 (println "digraph {")
                 (doseq [n (nodes g)]
                   (println (name n) (format "[color=\"%s\"]"
                                             (if (removable? g n)
                                               "green"
                                               "red"))))
                 (doseq [ [k vs]  (:-> g)
                         v vs
                         ]
                   (println (name k) "->" (name v))
                   )
                 (println "}")))

(comment
  (vg (graph (:supports (stack-cubes (user/day-input))))))
