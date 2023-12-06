(ns adventofcode.2015.day17)


(def example [20 15 10 5 5 ])


(def sample (frequencies example))

(def input [11
30
47
31
32
36
3
1
5
3
32
36
15
11
46
26
28
1
19
3
])



(def cache (atom {}))

(defn counts [limit [h & t :as bottles]]
  (cond (zero? limit) 1
        (neg? limit) 0
        (empty? bottles) 0
        :else
        (if-let [ cached (get-in @cache [limit bottles])]
          cached
          (let [res (+
                     (counts  (- limit h) t)
                     (counts  limit t))]
            (when (>= res 0)
              (swap! cache assoc-in [limit bottles] res))
            res))))


(counts 25 example)

cache


(defn part-1 [limit input]
  (reset! cache {})
  (counts limit input))



(part-1 150 input)4372


(defn possible [used-bottles limit [h & t :as bottles]]
  (cond (zero? limit) [used-bottles]
        (neg? limit) []
        (empty? bottles) []
        :else
        (let [res (concat
                   (possible (conj used-bottles  h)  (- limit h) t)
                   (possible  (conj used-bottles)  limit t))]

          res)))


(def)

{:part2
 (let [ a (possible [] 150 input)
       min-containers (apply min (map count a))]
   (->> a
        (filter #(= min-containers (count % )))
        count))}
