(ns adventofcode.2023.day16
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def ex (slurp (io/resource "2023/day16example.txt")))

(defn parse [s]
  (let [lines  (str/split-lines s)]
    {:grid (->> lines
               (mapv vec))
     :h (count lines)
     :w (count (first lines))}))

(parse ex)

(def up [-1 0])
(def down [1 0])
(def right [0 1])
(def left [0 -1])

(def desc
  {up :up
   down :down
   left :left
   right :right})

(def horizontal #{left right})
(def vertical #{up down})

(defn move [pos dir] (mapv + pos dir))

(def reflect-dir
  {\/ {up right
       down left
       left down
       right up}
   \\ {up left
       down right
       left up
       right down}})

(defn next-states [grid pos dir]
  (let [c (get-in grid pos)
        next-dirs (cond

                    (or (= c \.)
                        (and (horizontal dir) (= c \-))
                        (and (vertical dir) (= c \|)))
                    [dir]

                    :else
                    (case c
                      (\/ \\) [(get-in reflect-dir [c dir])]
                      \- [left right]
                      \| [up down]))]
    (map (fn [dir] [(move pos dir) dir]) next-dirs)))


(defn traverse [{:keys [grid h w]} init-pos init-dir]
  (loop [states [ [init-pos init-dir]  ]
         seen #{}]
    (if-some [ [pos dir :as state] (first states)]
      (let [ [y x] pos]
        (cond
          (or (< x 0) (< y 0) (>= x w) (>= y h))
          (recur (rest states) seen)

          (seen state)
          (recur (rest states) seen)

          :else
          (recur (concat (next-states grid pos dir) (rest states) )
                 (conj seen state))))
      seen)))

(defn energized [traversal]
  (distinct (map first traversal)))

(count (energized (traverse (parse ex) [0 0 ] right)))

(defn init-args [h w]
  (concat
   (for [y (range 0 h)] [ [y 0] right])       ; left edge
   (for [y (range 0 h)] [ [y (dec w)] left ]) ; right edge
   (for [x (range 0 w)] [ [0 x] down])        ; top edge
   (for [x (range 0 w)] [ [(dec h) x] up])))


(defn pp-visited [{:keys [h w]} traversal]
  (let [visited (reduce (fn [acc [pos dir]]
                          (update acc pos (fnil conj #{}) dir))
                        {}
                        traversal)]
    (doseq [ y (range 0 h)]
      (println (apply str
                      (for [ x (range 0 w)]
                        (let [visited-dirs (get visited [y x])]
                          (cond
                            (= 1 (count visited-dirs))
                            ({up \^
                              down \v
                              left \<
                              right \>
                              }
                             (first visited-dirs))
                            (= 0 (count visited-dirs))
                            \.

                            :else (count visited-dirs)))))))))

(comment

  (let [e (parse ex)]
    (pp-visited e
                (traverse e [0 0 ] right)))

  (let [e (parse (user/day-input))]
    (pp-visited e
                (traverse e [0 0 ] right)))

  )

(let [{:keys [grid h w] :as input} (parse (user/day-input))]
  {:part-1 (time (count (energized (traverse input [0 0] right))))
   :part-2
   (time
    (->> (init-args h w)
         (pmap (fn [ [pos dir] ] (count (energized (traverse input pos dir)))))
         (apply max)))})
