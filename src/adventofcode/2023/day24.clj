(ns adventofcode.2023.day24
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.test :as t]
            [clojure.math :as math]))

(def ex
"19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3")


(defn parse-line [line]
  (let [ [pos vel] (str/split line #" @ ")]
    {:pos (mapv bigdec (mapv read-string (str/split pos #", ")))
     :vel (mapv bigdec (mapv read-string (str/split vel #", "))) }
    ))

(defn parse [input]
  (mapv parse-line (str/split-lines input)))#'adventofcode.2023.day24/parse

(defn v- [a b] (mapv - a b))

(defn v+ [a b] (mapv + a b))

(defn v-cross [ [ax ay] [bx by]]
  (- (* ax by)
     (* ay bx)))

(defn dot [ [ax ay] [bx by]]
  (+ (* ax bx) (* ay by)))

(defn vscal [ [x y] s]
  [(* s x) (* s y)])

(def M 100)

(defn rot [[x y]]
  [ (- y) x])


(rot [0 1])

(defn intersects-2d [ l r]
  (let [a (:pos l)
        da (vscal (:vel l) 10) ;; a*t + c

        b (:pos r)
        db (vscal (:vel r) 10) ;; b*t + d

        h (dot (v- b a) (rot b))
        f (dot (rot b) da)
        ]
    (v+ a
        (vscal da (with-precision 10  (/ h f))))))

(mapv float (intersects-2d ((parse ex) 0)
                           ((parse ex) 1)))

(mapv float (intersects-2d-0 ((parse ex) 0)
                            ((parse ex) 1)))


(defn mag [[x y]]
  (math/sqrt (+ (* x x) (* y y))))

(defn intersects-2d-0 [ a b]

  (let [A (:pos a)
        B (:pos b)

        dA (:vel a)
        dB (:vel b)

        rdA (rot dA)
        f (dot dA (rot dB))

        ]
    (if (zero? f) ;; same direction
      nil
      (let [sA (/ (dot (v- B A) (rot dB) )
                  (dot dA (rot dB)))
            sB (/ (dot (v- A B) (rot dA) )
                  (dot dB (rot dA)))]
        ;; both hailstones need to travel forward
        (when (and (pos? sA) (pos? sB) )
          ;(v+ B (vscal dB sB)) ;; should give same result
          (v+ A (vscal dA sA)))))))



(mapv float (intersects-2d-0
             ((parse ex) 0)
             ((parse ex) 1)))

(assert (= (mapv double (intersects-2d-0
                        (parse-line "19, 13, 30 @ -2, 1, -2")
                        (parse-line "20, 25, 34 @ -2, -2, -4")))
           [11.66666666666667 16.66666666666667]
           ))

(mapv double (intersects-2d-0
              (parse-line "19, 13, 30 @ -2, 1, -2")
              (parse-line "20, 19, 15 @ 1, -5, -3")))

(intersects-2d-0
 (parse-line "18, 19, 22 @ -1, -1, -2")
 (parse-line "20, 25, 34 @ -2, -2, -4"))


(intersects-2d-0
 {:pos [1 1]
  :vel [1 0]}
 {:pos [-2 -2]
  :vel [1 1]}
 )

(defn combs [xs]
  (for [i (range 0 (dec (count xs)))
        j (range (inc i) (count xs))
        ]
    [(get xs i) (get xs j)]))


(defn in-test-area? [ low high [x y]]
  (and (<= low x high)
       (<= low y high)))

(defn counting [f xs]
  (count (filter identity  (map f xs))))


(defn part-1 [low high input]
  (let [ lms  (parse input)]
    (for [ [a b] (combs lms)

          :let [v (intersects-2d-0 a b)]
          :when v]
      [a b (mapv float v) (in-test-area? low high v)]
      )))


(intersects-2d-0
 (parse-line "12, 31, 28 @ -1, -2, -1")
 (parse-line "20, 19, 15 @ 1, -5, -3"))

(counting last
       (part-1 7 27 ex))


(intersects-2d-0
 {:pos [20 25 34], :vel [-2 -2 -4]}
  {:pos [20 19 15], :vel [1 -5 -3]})

(counting last (part-1 200000000000000
                       400000000000000
                       (user/day-input)))
