(ns adventofcode.2025.day08
  (:require
   [clojure.math :as m]
   [adventofcode.utils :as u]
   [clojure.string :as str]))

(def ex
  "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

(defn parse-line [l]
  (read-string (format "[%s]" l)))

(defn parse [x]
  (mapv parse-line (str/split-lines x)))

(defn dot [a b]
  (reduce + (map * a b)))

(defn distance [a b]
  (let [ab (map - a b)]
    (m/sqrt (dot ab ab))))

(defn distance-matrix-diag [e]
  (for [a e
        b (drop (/ (count a) 2) e)
       :when (not= a b)]
    [a b (distance a b)]))

(count (distance-matrix-diag (parse ex)))

(count (distance-matrix-diag (parse (user/day-input))))



(defn canonical-edge [ [a b]]
  (vec (sort [a b])))

(defn take-edges [n-shortest distance-matrix ]
  (reduce
   (fn [ edge-set  [from to distance]]
     (if (= n-shortest (count edge-set))
       (reduced edge-set)
       (conj edge-set (vec (sort [from to])))))
   (sorted-set)
   (sort-by peek distance-matrix)))

(defn take-edges2 [n-shortest distance-matrix ]
  (take n-shortest (distinct
                    (map (comp canonical-edge)
                         (sort-by peek distance-matrix)))))




(=
 (take-edges 1000 (distance-matrix-diag (parse (user/day-input))))
 (take-edges2 1000 (distance-matrix-diag (parse (user/day-input)))))

(defn edges->adj [edges]
  (reduce
   (fn [acc [from to]]
     (-> acc
         (assoc-in [from to] :_)
         (assoc-in [to from] :_)))
   {}
   edges))

(clojure.test/deftest ex1
  (let [d (mapv pop (sort-by last  (distance-matrix-diag (parse ex))))]
    (clojure.test/is (d 0) [[162,817,812] [ 425,690,689]] )
    (clojure.test/is (d 1) [[162,817,812] [ 431,825,988]] )
    (clojure.test/is (d 2) [[906,360,560] [ 805,96,715]] )
    (clojure.test/is (d 2) [[431,825,988] [ 425,690,689]] )
    ))


(user/graphviz-digraph :neato
;               (println "concentrate=true");
               (println "overlap=false")
               (let [i (atom 0)
                     label (memoize (fn [a] (swap! i inc)))
                     edges (take-edges 10 (distance-matrix-diag (parse ex)))]
                 (doseq [ [from to ] edges]
                   (println (label from) " " (format "[label=\"%s\",shape=box]" from))0
                   (println (label to) " " (format "[label=\"%s\",shape=box]" to)))
                 (doseq [[from to ] edges]
                   (println (label from ) " -> " (label to))
                   (println (label to ) " -> " (label from)))))

(defn edge [a b]
  (vec (sort [a b])))

(defn find-cycle-dfs [g start]
  (loop [todo [start]
        visited-edges #{}
        path []
        ]
    (if-not (seq todo)
      (pop path)
      (let [cur (first todo)
            nexts (for [trg  (keys (g cur))
                        :when (not (visited-edges (edge cur trg)))]
                    trg)]
        (recur
         (concat nexts (rest todo))
         (if (seq path)
           (conj visited-edges (edge cur (peek path)))
           visited-edges)
         (conj path cur))))))

(find-cycle-dfs {1 {2 'x}
                 2 {3 'y}
                 3 {1 'z}}
                3)

(defn remove-nodes [g nodes]
  (reduce
   (fn [acc [a b c]]
     (assoc-in acc [a b] c))
   {}
   (for [[src trg->v] g
         :when (not (nodes src))
         [trg v] trg->v
         :when (not (nodes trg))]
     [src trg v])))

(defn find-cycles [g]
  (if-not (seq g)
    []
    (let [ cycle (set (find-cycle-dfs g (ffirst g)))]
      (cons cycle (find-cycles (remove-nodes g cycle))))))


(defn cycles->cycle-map [ cycles]
  {:edges #{}
   :node->cycle
   (into {} (for [[i cycle] (map list (range) cycles)
                  node cycle]
              [node i]
              ))
   :cycle->nodes
   (reduce (fn [acc [i n]]
             (update acc i (fnil conj #{}) n))
           {}
           (for [[i cycle] (map list (range) cycles)
                 node cycle]
             [i node]
             ))})



(defn add-edge [{:keys [node->cycle cycle->nodes edges i] :as in} [a b]]
  (let [ ca (node->cycle a)
        cb (node->cycle b)
        next-i (inc (reduce max 0 (keys cycle->nodes)))]
    (cond (and (nil? ca) (nil? cb))     ; new cycle
          {:node->cycle (assoc node->cycle
                               a next-i
                               b next-i)
           :cycle->nodes (assoc cycle->nodes next-i #{a b})
           :edges (conj edges [a b])
           :i next-i}
          (nil? ca)                 ; extend cycle containing b with a
          {:node->cycle (assoc node->cycle
                               a cb)
           :cycle->nodes (-> cycle->nodes
                             (update cb conj a))
           :edges (conj edges [a b])
           :i i}
          (nil? cb)                     ; extend cycle a
          {:node->cycle (assoc node->cycle b ca)
           :cycle->nodes (-> cycle->nodes
                             (update ca conj b))
           :edges (conj edges [a b])
           :i i}

          (= ca cb)                     ; edge already in a cycle
          in

          (not= ca cb)                 ; edge joining different cycles
          {:node->cycle (->>
                         (reduce
                          (fn [acc n]
                            (assoc acc n next-i))
                          node->cycle
                          (concat (cycle->nodes ca)
                                  (cycle->nodes cb))))
           :cycle->nodes (-> cycle->nodes (dissoc ca cb)
                             (assoc next-i
                                    (set (concat (cycle->nodes ca)
                                                 (cycle->nodes cb)))))
           :edges (conj edges [a b])
           :i next-i})))

(let [a (cycles->cycle-map (find-cycles {1 {2 'x}
                                         2 {3 'y}
                                         3 {1 'z}
                                         :a {:b 'z}
                                         :b {:a 'e}
                                         }))]
 ; (assert (= a (add-edge a 1 2)))
;  (add-edge a 'p 'q)
  (add-edge a [1 :a]))


(let [a (cycles->cycle-map (find-cycles {}))]
  (reductions add-edge
              a
              [[1 2]
               [2 3]
               [3 4]
               [5 6]
               [6 1]
               ]))

(def x (atom []))


(loop [g (cycles->cycle-map [])
       edges (distinct (map canonical-edge
                            (sort-by peek (distance-matrix-diag (parse ex)))))
       ]
  (swap! x conj g)
  (cond
    (empty? edges)
    :exit

    (not (< (count (:edges g)) 10))
    g

    :else
    (recur (add-edge g (first edges))
           (rest edges))))


(set (find-cycle-dfs (edges->adj (take-edges 10 (distance-matrix-diag (parse ex))))
                     [346 949 466]
                     ))

(map count (find-cycles (edges->adj (take-edges 10 (distance-matrix-diag (parse ex))))

                        ))


(assert (= 40  (reduce * (take 3 (sort-by - (map count (find-cycles (edges->adj (take-edges 10 (distance-matrix-diag (parse ex))))

                                                                   )))))))

(defn part-2 [ex & {:keys [n] :or {n 1000}}]
  (reduce * (take 3 (sort-by - (map count (find-cycles (edges->adj (take-edges n (distance-matrix-diag (parse ex))))))))))
