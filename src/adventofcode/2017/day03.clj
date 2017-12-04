(ns adventofcode.2017.day03
  (:require [midje.sweet :refer :all]))

(defn sqrt [x] (Math/sqrt x))

(defn abs [x] (if (neg? x) (- x) x))

(def example
  [[17 16 15 14 13]
   [18  5  4  3 12]
   [19  6  1  2 11]
   [20  7  8  9 10]
   [21 22 23 24 25]])

(defn circles-to-origin
  [n]
  (-> n
      sqrt
      inc
      (/ 2)
      Math/ceil
      long
      dec))

(fact "about circles-to-origin"
  (let [d [0

           1 1 1
           1   1
           1 1 1

           2 2 2 2 2
           2       2
           2       2
           2       2
           2 2 2 2 2]]
    (map circles-to-origin (map inc (range 0 (count d))))
    =>
    d

    (mapv (partial mapv circles-to-origin) example)
    [[2 2 2 2 2]
     [2 1 1 1 2]
     [2 1 0 1 2]
     [2 1 1 1 2]
     [2 2 2 2 2]]))

(defn distance-from-axis [n]
  (if (= n 1)
    0
    (let [ci (circles-to-origin n)
          stride (-> ci (* 2) inc)
          prev-stride (- stride 2)
          ;; index of cell at start of current circle
          c-first (inc (* prev-stride prev-stride))
          ;; steps since start of swirl
          o (inc (- n c-first))
          axis-center  (/ (dec stride) 2)
          ;; distance from midpoint
          dm (abs (- (mod o (dec stride)) axis-center ))]
      dm)))

(fact "distance-from-axis works for example"
  (mapv (partial mapv distance-from-axis) example)
  =>
  [[2 1 0 1 2]
   [1 1 0 1 1]
   [0 0 0 0 0]
   [1 1 0 1 1]
   [2 1 0 1 2]])

;; fast & constant in time and space, but not extendable to part 2
(defn f3a [n]
  (+ (circles-to-origin n)
     (distance-from-axis n)))

(fact "f3a works for example"
  (mapv (partial mapv f3a) example)
  =>
  [[4 3 2 3 4]
   [3 2 1 2 3]
   [2 1 0 1 2]
   [3 2 1 2 3]
   [4 3 2 3 4]])

(fact "f3a has expected results for example"
  (mapv f3a [1 12 23 1024])
  => [0 3 2 31])

(defn f3 [part n]
  (loop [visited {}
         [x y :as prev-pos] [0 0]
         [dx dy :as prev-dir] [1 0] ; to the right
         i 1]
    (let [sum-of-neigbours (when (= part :b) ; skip for part a, to avoid int overflow error
                             (if (= i 1)
                               1 ; special case for first elem
                               (apply + (for [ox [-1 0 1]
                                              oy [-1 0 1]]
                                          (get visited (map + prev-pos [ox oy]) 0)))))

          nvisited (assoc visited prev-pos sum-of-neigbours)

          to-left [(- dy) dx] ; direction left of current direction: 90 degrees ccw rotation

          dir (cond
                ;; next block is taken: turn right
                (or (= i 1) (contains? nvisited (mapv + prev-pos prev-dir)))
                [1 0]
                ;; block to the left of current dir is taken: keep going in same dir
                (contains? nvisited (mapv + prev-pos to-left))
                prev-dir
                ;; turn left
                :else
                to-left)]
      (cond (and (= part :b) (> sum-of-neigbours n))
            sum-of-neigbours

            (and (= part :a) (= i n))
            (apply + (map abs prev-pos))

            :else
            (recur nvisited
                   (mapv + prev-pos dir)
                   dir
                   (inc i))))))

(fact "f3 works for part 2 example"
  (let [ex [[147  142  133  122   59]
            [304    5    4    2   57]
            [330   10    1    1   54]
            [351   11   23   25   26]
            [362  747  806]]]
    (for [row ex]
      (for [cell row]
        (f3 :b (dec cell))))
    =>
    ex))

{:part-1-fast (f3a 312051)
 :part-1 (f3 :a 312051)
 :part-2 (f3 :b 312051)}
