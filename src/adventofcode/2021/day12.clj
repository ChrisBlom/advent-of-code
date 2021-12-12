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

(defn small? [^String cave]
  (every? #(Character/isLowerCase ^char %) cave))

(defn start-or-end? [cave]
  (case cave
    ("start" "end") true
    false))

(defn make-target [x]
  {:cave x
   :is-small (small? x)
   :is-start-or-end (start-or-end? x)})

(defn transitions [input]
  (->> input
       str/split-lines
       (reduce (fn [acc line]
                 (let [ [from to] (str/split line #"-")]
                   (-> acc
                       (update from (fnil conj #{}) (make-target to))
                       (update to (fnil conj #{}) (make-target from)))))
               {})))

(defn next-states-1 [transitions {:keys [pos visited]}]
  (for [{:keys [cave is-small] :as target} (transitions pos)
        :when (if is-small
                (< (visited cave 0) 1)
                true)]
    {:pos (:cave target)
     :visited (update visited cave (fnil + 0) 1)}))

(defn next-states-2 [transitions {:keys [pos visited used-small-cave-twice]}]
  (for [{:keys [cave is-small] :as target} (transitions pos)
        :let [visits (visited cave 0)
              use-small-cave-twice (and is-small
                                        (= visits 1)
                                        (not used-small-cave-twice)
                                        (not (:is-start-or-end target)))]
        :when (if is-small
                (< visits (if use-small-cave-twice 2 1))
                true)]
    {:pos cave
     :visited (if is-small (assoc visited cave (inc visits)) visited)
     :used-small-cave-twice (if use-small-cave-twice cave used-small-cave-twice)}))

(assert (=  (next-states-2 {"a" (map make-target ["b" "c"])}
                           {:pos "a" :visited {"b" 1} :path []})
            '({:pos "b", :visited {"b" 2}, :used-small-cave-twice "b"}
              {:pos "c", :visited {"b" 1, "c" 1}, :used-small-cave-twice nil})))
(assert (=  (next-states-2 {"a" (map make-target ["b" "c"])}
                           {:pos "a" :visited {"b" 1} :path [] :used-small-cave-twice "b"})
            '({:pos "c", :visited {"b" 1, "c" 1}, :used-small-cave-twice "b"})))
(assert (=  (next-states-2 {"a" (map make-target ["b" "c" "start"])}
                           {:pos "a" :visited {"b" 1 "start" 1} :path []})
            '({:pos "b", :visited {"b" 2, "start" 1}, :used-small-cave-twice "b"}
              {:pos "c", :visited {"b" 1, "start" 1, "c" 1}, :used-small-cave-twice nil})))

(defn paths [transitions next-states]
  (loop [ [state & todo] [{:pos  "start" :visited {"start" 1}}]
         completed-paths 0]
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
