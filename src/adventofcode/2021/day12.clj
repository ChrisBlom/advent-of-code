(ns adventofcode.2021.day12
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(set! *warn-on-reflection* true)

(def example
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def example-2
"dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(def example-3
  "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(defn transitions [input]
  (->> input
       str/split-lines
       (reduce (fn [acc line]
                 (let [ [from to] (str/split line #"-")]
                   (-> acc
                       (update from (fnil conj #{}) to)
                       (update to (fnil conj #{}) from))))
               {})))

(defn small? [^String cave]
  (every? #(Character/isLowerCase ^char %) cave))

(defn next-states-1 [transitions {:keys [pos visited]}]
  (for [target (transitions pos)
        :when (if (small? target)
                (< (visited target 0) 1)
                true)]
    {:pos target
     :visited (update visited target (fnil + 0) 1)}))

(defn next-states-2 [transitions {:keys [pos visited small-cave-twice] :as state}]
  (for [target (transitions pos)
        :let [visits (visited target 0)
              is-small (small? target)
              use-small-cave-twice (and is-small
                                        (= visits 1)
                                        (not small-cave-twice)
                                        (not (#{"start" "end"} target)))]
        :when (if is-small
                (< (visited target 0) (if use-small-cave-twice 2 1))
                true)]
    {:pos target
     :visited (if is-small (update visited target (fnil + 0) 1) visited)
     :small-cave-twice (if use-small-cave-twice target small-cave-twice)}))

(next-states-2 {"a" ["b" "c"]}
               {:pos "a" :visited {"b" 1} :path [] :small-cave-twice nil})

(next-states-2 {"a" ["b" "c"]}
               {:pos "a" :visited {"b" 1} :path [] :small-cave-twice "b"})

(next-states-2 {"a" ["b" "c" "start"]}
             {:pos "a" :visited {"b" 1 "start" 1} :path []})

(defn paths [transitions next-states]
  (loop [ [state & todo] [{:pos  "start" :visited {"start" 1}}]
         completed-paths 0]
;    (println (:pos state) (count todo) (count completed-paths))
    (cond
      (nil? state)
      completed-paths

      (= (:pos state) "end")
      (recur todo (inc completed-paths))

      :else
      (recur (concat (next-states transitions state) todo)
             completed-paths))))

(assert (= 10 (paths (transitions example) next-states-1)))
(assert (= 19 (paths (transitions example-2) next-states-1)))

(assert (= 36 (paths (transitions example) next-states-2)))
(assert (= 103 (paths (transitions example-2) next-states-2)))
(assert (= 3509 (paths (transitions example-3) next-states-2)))

{:part-1
 (time (paths (transitions  (slurp (io/resource "2021/day12.txt"))) next-states-1 ))
 :part-2
 (time (paths (transitions  (slurp (io/resource "2021/day12.txt"))) next-states-2 ))}
