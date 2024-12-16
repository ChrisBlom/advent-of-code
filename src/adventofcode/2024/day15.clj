(ns adventofcode.2024.day15
  (:require
   [adventofcode.utils :as u :refer [v+]]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

(defn parse [x]
  (let [ [grid-lines _ move-lines] (partition-by str/blank? (str/split-lines x))
        grid (mapv vec grid-lines)
        start-pos (first  (keep (fn [ [pos v] ]
                                  (when (= v \@)
                                    pos))
                                (u/grid-seq grid)))]
    {:grid (assoc-in  grid start-pos \.)
     :instructions (apply concat (map seq move-lines))
     :pos start-pos }))

(parse ex)

(def dir->vec (zipmap "^>v<" [u/up u/right u/down u/left]))

(defn box-positions [grid pos dir]
  (u/take-while+
   (fn [p] (= \O (get-in grid p)))
   (rest (iterate (partial u/v+ dir) pos ))))


(defn move-boxes [grid dir box-positions]
  (let [cleared
        (reduce (fn [acc p]
                  (assoc-in acc p \.))
                grid
                box-positions)]
    (reduce (fn [acc p]
              (assoc-in acc (v+ p dir) \O))
            cleared
            box-positions)))

(defn step [{:keys [pos grid instructions] :as a}]
  (when-some [dir (first instructions)]
    (let [dirv (dir->vec dir)
          bps (box-positions grid pos dirv)
          at-lpos (get-in grid (last bps))]
      (if (or (nil? at-lpos) (= at-lpos \#))
        (-> a
            (update :instructions rest)) ; can't move into wall
        {:pos (u/v+ pos dirv)
         :grid (move-boxes grid dirv (butlast bps))
         :instructions (rest instructions)}))
    ))



(defn gps [ [y x]]
  (+ (* 100 y)
     x))

(defn score [{:keys [grid]}]
  (reduce +
          (keep
           (fn [ [pos v]]
             (when (#{\O \[} v)
               (gps pos)))
           (u/grid-seq grid))))



(defn part-1 [x]
  (->> x
       parse
       (iterate step)
       (take-while some?)
       last
       score))

(assert (= 2028 (part-1 ex)))


(defn widen [{:keys [grid pos instructions]}]
  (let [wide-grid (mapv (fn [line]
                          (vec (mapcat (fn [cell]
                                         (case cell
                                           \# "##"
                                           \O "[]"
                                           \. ".."
                                           \@ "@.")) line)))
                        (assoc-in grid pos \@))
        start-pos (first (keep (fn [ [pos v]] (when (= v \@) pos)) (u/grid-seq wide-grid)))]
    {:grid (assoc-in wide-grid start-pos \.)
     :pos start-pos
     :instructions instructions}))


(defn pp [{:keys [pos grid] :as s}]
  (println "--------------------------------------------------------------------------------")
  (doseq [line (assoc-in grid pos \@)]
    (println (str/join line )))
  s)

(defn combine [a b]
  (when (and a b)
    (concat a b)))

(defn wide-box-positions [grid pos dir]
  (let [npos1 (u/v+ pos dir)
        npos2 (u/v+ npos1 dir)
        npos3 (u/v+ npos2 dir)]
    (cond
      (= dir u/left)
      (when (and (= (get-in grid npos1) \])
                 (= (get-in grid npos2) \[))
        (case (get-in grid npos3)
         ;;.[]@
          \. [[npos2 npos1]  ]
          ;;#[]@
          \# nil
         ;;  ][]@
          \] (when-let [nboxes (wide-box-positions grid npos2 dir)]
               (cons [npos2 npos1] nboxes))))

      (= dir u/right)
      (when (and (= (get-in grid npos1) \[)
                 (= (get-in grid npos2) \]))
        (case (get-in grid npos3)
         ;;.[]@
          \. [[npos1 npos2]  ]
          ;;#[]@
          \# nil
         ;;  ][]@
          \[ (when-let [nboxes (wide-box-positions grid npos2 dir)]
               (cons [npos1 npos2] nboxes))))

      (or (= dir u/up)
          (= dir u/down))
      (case (get-in grid npos1)
        \. []
        \# nil
        ;;;  xx
        ;;;  []
        ;;;   @
        \] (if-let [nboxes (combine
                            (wide-box-positions grid (v+ u/left npos1) dir)
                            (wide-box-positions grid npos1 dir))]
             (cons [(v+ u/left npos1) npos1]
                   nboxes))
        \[ (if-let [nboxes (combine
                            (wide-box-positions grid npos1 dir)
                            (wide-box-positions grid (v+ u/right npos1) dir))]
             (cons [npos1 (v+ u/right npos1)]
                   nboxes)
             )))))

(wide-box-positions (:grid (parse "#.[][]@."))
                    [0 6]
                    u/left)

(wide-box-positions (:grid (parse "##[][]@."))
                    [0 6]
                    u/left)


(wide-box-positions (:grid (parse "##[][]@[][]."))
                    [0 6]
                    u/right)

(wide-box-positions (:grid (parse "##[][]@[][]#"))
                    [0 6]
                    u/right)

(wide-box-positions (:grid (parse "#######
#.......
#.[]....
#[][]...
#.[]....
#..@...."))
                    [5 3 ]
                    u/up)

(wide-box-positions (:grid (parse "#######
#.@.....
#.[]....
#[][]...
#.[]...
#......
#######"))
                    [1 2]
                    u/down)




(widen (parse ex))





(defn move-wide-boxes [grid dir wide-box-positions]
  (let [cleared
        (reduce (fn [acc [lp rp]]
                  (-> acc
                      (assoc-in lp \.)
                      (assoc-in rp \.)))
                grid
                wide-box-positions)]
    (reduce (fn [acc [lp rp]]
                (-> acc
                    (assoc-in (v+ lp dir) \[)
                    (assoc-in (v+ rp dir) \])))
            cleared
            wide-box-positions)))


(defn wide-step [{:keys [pos grid instructions] :as a}]
  (when-some [dir (first instructions)]
    (let [dirv (dir->vec dir)
          bps (wide-box-positions grid pos dirv)
          npos (u/v+ pos dirv)]
      (cond
        (= \. (get-in grid npos))
        (-> a
            (update :instructions rest)
            (assoc :pos npos))

        (empty? bps)
        (-> a
            (update :instructions rest)) ; can't move into wall

        :else
        {:pos (u/v+ pos dirv)
         :grid (move-wide-boxes grid dirv bps)
         :instructions (rest instructions)}))
    ))


(defn part-2 [x]
  (->> x
       parse
       widen
       pp
       (iterate wide-step)
       (take-while some?)
       last
       pp
       score))


(def ex-3 "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^")

(-> ex-3
    parse
    widen
    wide-step
    pp
    wide-step
    pp
    wide-step
    pp
    wide-step
    pp
    wide-step
    pp
    wide-step
    pp
    wide-step
    pp

    )


(def ex-large "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")


(assert (= (part-2 ex-3)))

(assert (= 9021 (part-2 ex-large)))


{:part-1 (part-1 (user/day-input))
 :part-2 (time (part-2 (user/day-input)))}


(comment



  )
