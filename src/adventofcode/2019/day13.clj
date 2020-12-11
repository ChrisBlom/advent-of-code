(ns adventofcode.2019.day13
  (:require [adventofcode.2019.intcode :as intcode]
            [clojure.java.io :as io]
            [mikera.image.core :as img]))

(def paddle 3)
(def ball 4)

(def input (->>
            "2019/day13.txt"
            io/resource
            slurp
            (format "[%s]")
            read-string))

(defn pixel [tile]
  (case tile
    nil \space
    0 \_
    1 \|
    2 \█
    3 \=
    4 \@))

{:part-1
 (->>
  (intcode/run-intcode input [])
  :out
  (partition 3)
  (filter (comp #{2} last))
  count)}

(defn segment [[x y score]]
  (if (= [x y] [-1 0])
    score))

(defn process-instruction [game-state [x y tile]]
  (if (= [x y] [-1 0])
    (assoc game-state :score tile)
    (cond-> (assoc-in game-state [:screen y x] tile)
      (= tile ball) (assoc :ball-pos [x y])
      (= tile paddle) (assoc :paddle-pos [x y]))))

(reduce process-instruction {:screen {}}
        [[0 0 1]
         [1 0 1]
         [7 7 ball]
         [8 8 paddle]
         [-1 0 1337]])

(defn step-game [{:keys [intcode-state score step ball-pos paddle-pos] :as game-state}]
  (let [intcode-with-io (assoc intcode-state
                               :out []
                               :await-input false ;; TODO let intcode set this
                               :in (if (and ball-pos paddle-pos)
                                     (list (compare (ball-pos 0) (paddle-pos 0)))
                                     (list 0)))
        next-intcode-state (intcode/run-intcode- intcode-with-io
                                                 :halt? (some-fn :await-input intcode/halted?))
        instructions (partition 3 (:out next-intcode-state))]
    (reduce process-instruction
            (assoc  game-state
                    :intcode-state next-intcode-state
                    :step (inc step))
            instructions)))

(defn show [states]
  (let [img (img/new-image 44 24)]
    (doseq [{:keys [screen] :as s} states]
      (Thread/sleep (long 1000/24))
      (let [ys (keys screen)
            xs (mapcat keys (vals screen))]
        (doseq [y (range (apply min ys) (inc (apply max ys)))
                x (range (apply min xs) (inc (apply max xs)))]
          (def _c [x y])
          (img/set-pixel img x y
                         (case (get-in screen [y x])
                           nil 0xFF000000
                           0 0xFF000000
                           1 0xFFFFFFFF
                           2 0xFFCCCCCC
                           3 0xFFFF0000
                           4 0xFFFFFFFF))))
      (img/show img :resize  10))
    {:n (count states)
     :score (map :score states)}))

(time
    {:part-2
     (let [initial-state (-> input
                             (intcode/init-state [])
                             (assoc-in [:buffer 0] 2))]
       (->> (iterate step-game {:intcode-state initial-state
                                :screen {}
                                :step 0})
            rest
            (drop-while (comp not intcode/halted? :intcode-state))
            first
            :score))})

#_(let [
      initial-state (-> input
                        (intcode/init-state [])
                        (assoc-in [:buffer 0] 2))]
  (->> (iterate step-game {:intcode-state initial-state
                           :screen {}
                           :step 0})
       rest
       (take-while #(not (intcode/halted? (:intcode-state %))))
       show


                                       ;       (map :score)
                                        ;       (map print-grid)
                                        ;       (map keys)
       ))
