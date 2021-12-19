(ns adventofcode.2021.day18
  (:use midje.sweet)
  (:require
   [clojure.string :as str]
   [clojure.zip :as z]
   [clojure.java.io :as io]))

(defn z-depth [loc]
  (assert (meta loc))
  (if-let [parent (z/up loc)]
    (inc (z-depth parent))
    0))

(defn find-loc [loc pred]
  (if (z/end? loc)
    nil
    (if (pred loc)
      loc
      (find-loc (z/next loc) pred))))

(defn locate-explode-pair
  "find leftmost pair with depth >= 4"
  [loc]
  (find-loc loc #(and (vector? (z/node %)) (>= (z-depth %) 4))))

(fact
  (z/node (locate-explode-pair (z/vector-zip [[6,[5,[4,[3,2]]]],1]))) => [3 2]
  (z/node (locate-explode-pair (z/vector-zip [[6,[5,[4,[3,2]]]],[ 1 [ 2 [3 [4 [5 [6 7] ]]]]]]))) => [3 2])

(defn edit-left-number
  "replace number left of loc with result of (f node arg) and move back to loc"
  [loc f arg]
  (assert (number? arg))
  (if-let [l (z/prev loc)]
    (z/next (if (number? (z/node l))
              (z/edit l f arg)
              (edit-left-number l f arg)))
    loc))

(defn edit-right-number
  "replace number right of loc with result of (f node arg) and move back to loc"
  [loc f arg]
  (assert (number? arg))
  (if-let [r (z/next loc)]
    (if (z/end? r)
      loc
      (z/prev (if (number? (z/node r))
                (z/edit r f arg)
                (edit-right-number r f arg))))
    loc))

(defn explode [x]
  (when-let [loc-to-explode (locate-explode-pair (z/vector-zip x))]
    (let [[l r] (z/node loc-to-explode)]
      (->
       loc-to-explode
       (z/replace 0)
       (edit-left-number + l)
       (edit-right-number + r)
       (z/root)))))

(fact
  (explode [[[[[9,8],1],2],3],4])
  => [[[[0,9],2],3],4]
  (explode [7,[6,[5,[4,[3,2]]]]])
  => [7,[6,[5,[7,0]]]]
  (explode [[6,[5,[4,[3,2]]]],1])
  => [[6,[5,[7,0]]],3]
  (explode [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])
  => [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
  (explode [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]])
  => [[3,[2,[8,0]]],[9,[5,[7,0]]]])

(defn split-num [loc]
  (let [n (z/node loc)]
    (assert (>= n 10))
    (let [l (int (/ n 2))
          r (- n l)]
      (z/replace loc [l r]))))

(defn split [x]
  (when-let [l (find-loc (z/vector-zip x) #(and (number? (z/node %)) (>= (z/node %) 10)))]
    (z/root (split-num l))))

(fact
  (let [a [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
        b (explode a)
        c (explode b)
        d (split c)
        e (split d)
        f (explode e)  ]
b => [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
c => [[[[0,7],4],[15,[0,13]]],[1,1]]
d => [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
e => [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
f => [[[[0,7],4],[[7,8],[6,0]]],[8,1]]))

(defn magnitude [x]
  (if (number? x)
    x
    (let [[l r] x]
      (+ (* 3 (magnitude l))
         (* 2 (magnitude r))))))

(fact
  (magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])
  => 3488)


(defn snailfish-reduce [n]
  (if-let [x (or (explode n) (split n))]
    (if (= x n)
      n
      (snailfish-reduce x))
    n))

(fact
  (snailfish-reduce [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]])
  => [[[[0 7] 4] [[7 8] [6 0]]] [8 1]])

(defn fish-add [a b]
  (snailfish-reduce [a b]))

(defn final-sum [xs]
  (reduce fish-add xs))

(fact
  (final-sum [[1,1] [2,2] [3,3] [4,4] ])
  => [[[[1,1],[2,2]],[3,3]],[4,4]]

  (final-sum
   [[1,1]
    [2,2]
    [3,3]
    [4,4]
    [5,5]
    [6,6]])
  => [[[[5,0],[7,4]],[5,5]],[6,6]])

(def big-example
  [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
   [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
   [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
   [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
   [7,[5,[[3,8],[1,4]]]]
   [[2,[2,2]],[8,[8,1]]]
   [2,9]
   [1,[[[9,3],9],[[9,0],[0,7]]]]
   [[[5,[7,4]],7],1]
   [[[[4,2],2],6],[8,7]]])

(fact
  (final-sum big-example)
  => [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])

(def input  (read-string (format "[%s]" (slurp (io/resource "2021/day18.txt")))))

{:part-1 (magnitude (final-sum input))
 :part-2 (time (reduce max
                      (for [i (range 0 (count input))
                            j (range 0 (count input))
                            :when (not= i j)]
                        (magnitude (fish-add (input i)
                                             (input j))))))}
