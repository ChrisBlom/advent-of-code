(ns adventofcode.2023.day12
  (:require [clojure.string :as str]))



(require '[automat.viz :refer (view)])
(require '[automat.core :as a])
(require '[automat.fsm :as fsm])



(def ex1 "???.### 1,1,3")

(defn parse-line [x]
  (let [ [springs groups] (str/split x #" ")
        groups (read-string (str  "[" groups "]"))]
    {:springs springs
     :groups groups}))




(defn make-last-final [g]
  (update g :final (fnil conj #{}) (:prev g)))

(defn make-prev-last-final [g]
  (update g :final conj (dec  (:prev g))))

(def init{ :initial #{0}
           :final #{}
           :rel (sorted-map)
           :prev 0
           })

(defn add-transition-choice [{:keys [rel prev] :as g} & cs]
  (let [nxt (inc prev)]
    (assoc g
           :rel (reduce (fn [rel- c] (assoc-in rel- [prev c] nxt ))
                        rel
                        cs)

           :prev nxt)))

(defn springs-nfa-2 [string]
  (reduce (fn [acc c]
            (case c
              \? (concat acc (a/or \. \#))
              \. (concat acc [\.])
              \# (concat acc [\#])))
          []
          string))

(springs-nfa-2 "..??##")


(defn springs-nfa [string]
  (->> string
       (reduce
        (fn [{:keys [rel prev] :as g} c]
          (let [nxt (inc prev)]
            (assoc g
                   :rel (case c
                          \? (-> rel
                                 (assoc-in [prev \.] nxt )
                                 (assoc-in [prev \#] nxt )
                                 )
                          \. (-> rel
                                 (assoc-in [prev \.] nxt )
                                 )
                          \# (-> rel
                                 (assoc-in [prev \#] nxt )
                                 ))
                   :prev nxt
                   )))
        init)
       make-last-final))

(defn add-transition [{:keys [rel prev] :as g} c]
  (let [nxt (inc prev)]
    (assoc g
           :rel (assoc-in rel [prev c] nxt )
           :prev nxt)))


(defn add-edge [{:keys [rel prev] :as g} from c to]
  (let [nxt (inc prev)]
    (assoc g
           :rel (assoc-in rel [from c] to)
           :prev nxt)))


(defn add-+ [{:keys [rel prev] :as g} c]
  (let [nxt (inc prev)]
    (assoc g
           :rel (-> rel
                    (assoc-in [prev c] nxt )
                    (assoc-in [nxt c] prev ))
           :prev nxt)))

(defn add-* [{:keys [rel prev] :as g} c]
  (assoc g
         :rel (assoc-in rel [prev c] prev )
         :prev prev))

(defn add-string [rels chars]
  (reduce add-transition rels chars))

(defn group-nfa [ nums ]
  (-> (reduce
       (fn [g n]
         (->
          g
          (add-*  \.)
          (add-string  (repeat n \#))
          (add-+ \.)))
       init
       nums)
      make-last-final
      make-prev-last-final))

(require 'clojure.walk)

(defn to-dot [{:keys [rel prev final initial]}]
  (println "digraph {")
  (let [nodes (into (sorted-set) (concat (keys rel) (mapcat vals (vals rel))))
        pnode #(str \" % \")]
    (doseq [n nodes]
      (println (pnode n)  (format " [ shape=%s]" (cond
                                                   (final n) "diamond"
                                                   (initial n) "doublecircle"
                                                   :else "circle") )))
    (doseq [ [ src char->trg] (clojure.walk/postwalk-replace {:end prev} rel)
            [c trg] char->trg ]
      (println (pnode src) " -> " (pnode trg) (format " [ label = \"%s\" ] " c ))))
  (println "}"))

(require '[clojure.java.shell :as sh])

(defn export-graph [input]
  (spit "out.dot" (with-out-str (to-dot input)))
  (assert (zero? (:exit (sh/sh "dot" "-Tsvg" "out.dot" "-O"))))
  (assert (zero? (:exit (sh/sh "open" "out.dot.svg")))))

(defn relabel
  ([nfa] (relabel nfa 0))
  ([ {:keys [rel prev final initial]} n]
   (let [mapping (volatile! {})
         i (volatile! n)
         relabel (fn [a]
                   (if (contains? @mapping a)
                     (get @mapping a)
                     (let [l @i]
                       (vswap! mapping assoc a l)
                       (vswap! i inc)
                       l)))]
     (relabel [0 0])
     {      :initial (set (map relabel initial))
      :final (set (map relabel final))
      :rel (-> rel
               (update-keys relabel)
               (update-vals (fn [ c->trg] (update-vals c->trg relabel))))
      :prev (relabel prev)
})))

(defn tag-nodes
  ([ {:keys [rel prev final initial]} tag]
   (let [relabel (fn [a] {tag a})]
     {:rel (-> rel
               (update-keys relabel)
               (update-vals (fn [ c->trg] (update-vals c->trg relabel))))
      :prev (relabel prev)
      :initial (set (map relabel initial))
      :final (set (map relabel final))})))


(defn intersect-nfa [ left  right ]
  (loop [todo [ [0 0] ] ;;TOOD initials
         intersect (assoc init :initial (for [ il (:initial left)
                                              ir (:initial right)]
                                          [il ir]))]
    (if-some [ [nl nr :as nlr] (first todo) ]
      (let [next-l (get-in left [:rel nl])
            next-r (get-in right [:rel nr])
            options (distinct (concat (keys next-l)
                                      (keys next-r)))
            edges (for [c options
                        :let [next-l (get-in left [:rel nl c])
                              next-r (get-in right [:rel nr c])]
                        :when (and next-l next-r)]
                    [ nlr c [next-l next-r]])
            both-initial? (fn [ [l r]] (and ((:initial left)  l )
                                           ((:initial right) r )))
            both-final? (fn [ [l r]] (and ((:final left)  l )
                                         ((:final right) r )))
            next-intersect (reduce (fn [dfa- [s c t]]
                                     (add-edge  dfa- s c t))
                                   (-> intersect
                                       (update :final into (filter both-final? (map last edges)))
                                       (update :initial into (filter both-initial? (map last edges))))
                                   edges)]
        (recur
         (concat (map last edges) (rest todo))
         next-intersect))
      (relabel intersect))))

(export-graph
 (springs-nfa "?###????????"))

(export-graph
 (group-nfa [3]))

(export-graph
 (intersect-nfa (springs-nfa "?###????????")
                (group-nfa [3 1 1])))

(defn traverse-nfa [{:keys [rel final initial] :as nfa}]
  (loop [todo (for [n initial]
                [n []] )
         res []]
    (if-let [ [src path] (first todo)]
      (let [ c+trgs (rel src)
            succ (for [ [c trg] c+trgs]
                   [trg (conj path c)])
            reached-final (filter (comp final first) succ)
            ]
        (recur (concat succ (rest todo))
               (into res reached-final)))
      (map (comp str/join second) res))))

(defn traverse-nfa-count [{:keys [rel final initial] :as nfa}]
  (loop [todo (for [n initial]
                [n []] )
         res []]
    (if-let [ [src path] (first todo)]
      (let [ c+trgs (rel src)
            succ (for [ [c trg] c+trgs]
                   [trg (conj path c)])
            reached-final (filter (comp final first) succ)
            ]
        (recur (concat succ (rest todo))
               (into res reached-final)))
      (map (comp str/join second) res))))

(assert (= 10
           (count (traverse-nfa
                   (intersect-nfa (springs-nfa "?###????????")
                                  (group-nfa [3 2 1]))))))

(assert (= 10
           (count (traverse-nfa
                   (intersect-nfa (springs-nfa "?###????????")
                                  (group-nfa [3 2 1]))))))

(defn to-nfa [{:keys [springs groups] :as x}]
  (intersect-nfa (springs-nfa springs)
                 (group-nfa groups)))

(defn part-1-line [{:keys [springs groups] :as x}]
  (-> x
      to-nfa
      traverse-nfa
      count))

(defn invert-rel [rel]
  (reduce
   (fn [acc [s t c]]
     (assoc-in acc [t c] s))
   {}
   (for [ [s c->t] rel
         [c t] c->t]
     [s t c])))

(defn reverse-nfa [nfa]
  (let [rel- (invert-rel (:rel nfa))]
    (loop [ todo (:final nfa)
           rev (assoc init :initial #{})
           visited #{}
           ]
      (if-some [trg (first todo)]
        (if (visited trg)
          (recur (rest todo)
                 rev
                 visited)
          (let [ c->src (rel- trg)
                was-final ((:final nfa) trg)
                was-initial ((:initial nfa) trg)

                rel- (reduce
                      (fn [rel- [c src]]
                        (assoc-in rel- [trg c] src))
                      (:rel rev)
                      c->src)
                ]

            (recur
             (concat (vals c->src) (rest todo))
             (cond-> (assoc rev :rel rel-)
               was-final (update :initial conj trg)
               was-initial (update :final conj trg)
               )
             (conj visited trg))))
        rev
        ))))

(reverse-nfa (reverse-nfa (-> init (add-string  "123") make-last-final)))


(defn prune-nfa [nfa] (reverse-nfa (reverse-nfa nfa)))

(part-1-line (parse-line ex1))

(part-1-line (parse-line ""))

(def example "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defn part-1 [input]
  (->> input
       str/split-lines
       (map (comp part-1-line parse-line))
       (apply +)))

(assert (= 21 (part-1 example)))


(part-1 (user/day-input))

(defn unfold [{:keys [springs groups]}]
  {:springs (apply str  (interpose "?" (repeat 5 springs)))
   :groups (vec  (apply concat (repeat 5 groups)))})

(unfold (parse-line ".# 1"))

(defn concat-nfa- [a b]
  (let [l (tag-nodes a :l)
        r (tag-nodes a :r)
        connect       (zipmap (:final l) (:initial r))]
    {:initial (:initial l)
     :final (:final r)
     :rel (clojure.walk/postwalk-replace
           connect
           (merge-with merge
                       (:rel l)
                       (:rel r)))}))

(defn concat-nfa [a b]  (relabel (concat-nfa- a b)))

(export-graph
 (relabel
  (concat-nfa
   (-> init (add-string "abc") make-last-final)
   (-> init (add-string "def") make-last-final))))


(def a (first
        (->> (user/day-input)
             str/split-lines
             (map parse-line))))


(def ?-nfa
  (-> init
      (add-transition-choice \. \#)
      make-last-final
      ))

(export-graph
 (prune-nfa
  (let [{:keys [springs groups]} a
        snfa (springs-nfa springs)]
    (-/dd snfa)
    (export-graph (relabel (reduce concat-nfa [snfa ?-nfa snfa])))
         #_(intersect-nfa

     (relabel (reduce concat-nfa [(group-nfa groups) (group-nfa groups)]))))))


(export-graph
 (concat-nfa _snfa (concat-nfa ?-nfa _snfa)))

(doseq [l (take 1 (->> (user/day-input)
                       str/split-lines
                       (map parse-line)
                       unfold))]
  (do
    (let [nfa  (->> l
                    parse-line
                    unfold
                    to-nfa
                    prune-nfa
                    )]
      (export-graph nfa)
      (print l)
                                        ;      (println " => " (count  (traverse-nfa nfa)))
      )))



(defn part-2 [input]
  (->> input
       str/split-lines
       (pmap (comp part-1-line unfold parse-line))
       (apply +)))



(part-2 (user/day-input))






(parse-line "????.######..#####. 1,6,5")


(defn optimize [{:keys [springs groups]}]
  (let [sections (map (fn [x] {:type (first x)
                              :len (count x)}) (partition-by identity springs))
        defect-streaks (filter #(re-matches #"#+" %) sections)
        defect-streak-lens (map count defect-streaks)]
    [sections groups]))

4,1 => 4
4,2 => 3
3,3 => 2

????
x
 x
  x
   x

????
xx
 xx
  xx

????
xxx
 xxx

????
xxxx

(defn poss [qmarks defects]
  (- qmarks (dec defects)))

(poss 8 2)

(def ex "?#?#?#?#?#?#?#?")
from functools import cache

@cache
def recurse(lava, springs, result=0):
    if not springs:
        return '#' not in lava
    current, springs = springs[0], springs[1:]
    for i in range(len(lava) - sum(springs) - len(springs) - current + 1):
        if "#" in lava[:i]:
            break
        if (nxt := i + current) <= len(lava) and '.' not in lava[i : nxt] and lava[nxt : nxt + 1] != "#":
            result += recurse(lava[nxt + 1:], springs)
    return result

(def broken? #{\#})


(possibilities "..." [])
(possibilities "..#" [])

(possibilities "..#" [1])


(defn possibilities [s groups]
  (if-some [g (first groups)]
    (let [g (first groups)
          group-len (reduce + groups)
          group-len-with-spacing (+ group-len (dec (count groups)))]
      (->> (range 0 (- (count s) group-len-with-spacing))
           (map (fn [end]
                  (let [group-end (+ g end)
                        current (subvec s 0 group-end)]
                    (possibilities (subvec s group-end)
                                   (rest groups)))))))
    (if (some #{\# \?} s) 1 0)))



(defn possibilities [s groups]
  (if-some [g (first groups)]
    (->> so
         count
         (range 0)
         (map (fn [end]
                (let [group-end (+ g end)
                      current (subvec s 0 group-end)]
                  (*
                   (possibilities (subvec s 0 group-end)
                                  groups)
                   (possibilities (subvec s group-end)
                                  (rest groups)))))))
    (if (some #{\# \?} s) 1 0)))

(possibilities (vec "?..#") [1])

(possibilities (vec "?#") [1])

(possibilities (vec "???") [1])


(let [data (map #(str/split % #" ") (str/split-lines (user/day-input)))]
  (map
   (fn [ [lava springs] ]
     (recurse (vec lava) (read-string (str "[" springs"]"))))

   data))


with open("day12.txt", "r") as file:
    data = [x.split() for x in file.read().splitlines()]
    p1, p2 = 0, 0
    for e, (lava, springs) in enumerate(data):
        p1 += recurse(lava, (springs := tuple(map(int, springs.split(",")))))
        p2 += recurse("?".join([lava] * 5), springs * 5)
    print(p1, p2)



(defn subs* [row i]
  (if (< i (count row))
    (subs row i)
    ""))

(def separator? #{\. \?})
(def dam? #{\# \?})

(defn valid? [row b]
  (and (<= b (count row))
       (every? dam? (subs row 0 b))
       (or (= b (count row)) (sep? (nth row b)))))

(def solve (memoize (fn [row groups]
     (if-not (first groups)
       (if (every? separator? row) 1 0)
       (let [space (- (count row) (reduce + groups) (max 0 (dec (count groups))))]
         (if (< space 0)
           0
           (loop [[g & gs :as groups] groups
                  i 0
                  poss 0]
             (if (> i space)
               poss
               (let [poss (+ poss
                             (if (valid? (subs row i) g)
                               (solve (subs* row (+ i g 1)) gs)
                               0))]
                 (if (separator? (nth row i))
                   (recur groups (inc i) poss)
                   poss))))))))))

(defn parse-line [line]
  (let [[row bs] (str/split line #" ")]
    {:row row
     :groups (read-string (str "(" bs ")"))}))

(apply solve (parse-line "???? 1"))

(defn unfold [[row groups]]
  [(str/join \? (repeat 5 row)) (apply concat (repeat 5 groups))])


(->> (user/day-input)
     (str/split-lines)
     (map parse-line)
     (map     solve))
