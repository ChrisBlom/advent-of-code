(ns adventofcode.2019.day15
  (:require
   [adventofcode.2019.intcode :as intcode]
   [clojure.java.io :as io]
   [mikera.image.core :as img]))


(def program (->> (io/resource "2019/day15.txt")
                  slurp
                  (format "[%s]")
                  read-string))

(def direction-code
  {:north 1
   :south 2
   :west 3
   :east 4})

(def opposite
  {:north :south
   :south :north
   :west :east
   :east :west})

(def directions
  [:south :west :north :east]
  )

(def init-state {:dir nil
                 :result nil
                 :path []
                 :pos [0 0]
                 :grid {[0 0] :empty}
                 :intcode-state (intcode/init-state program)})



(do
  (def hit-wall 0)
  (def moved 1)
  (def found-oxygen-system 2))

(def delta
  {:north [0 1]
   :south [0 -1]
   :west [-1 0]
   :east [1 0]})

(defn move [pos dir]
  (mapv + pos (delta dir)))

(move [0 0] :north)

(defn run-io [intcode-state dir]
  (intcode/run-intcode- (assoc intcode-state
                               :in (list (direction-code dir))
                               :await-input false)
                        :halt?
                        :await-input))

(defn step [{:keys [intcode-state pos grid prev-dir path] :as state} dir]
  (let [new-state (run-io intcode-state dir)
        result (first (:out new-state))
        next-pos (move pos dir)
        discoved {next-pos  (case result
                              0 :wall
                              1 :empty
                              2 :oxygen-system)}]
    (-> state
        (assoc :result result
               :pos (case result
                      0 pos
                      1 next-pos
                      2 next-pos)
               :intcode-state new-state
               :grid (assoc grid next-pos  (case result
                              0 :wall
                              1 :empty
                              2 :oxygen-system) )))))

(-> init-state
    (step :south)
    (step :south))


(defn bfs [limit init-state next-states done? visited-key]
  (loop [ [state & t :as todo] [init-state]
         visited #{}
         i 0
         ]
    (cond
      (> i limit) {:exhaust state :visited visited}
      (not (seq todo)) :fail
      (done? state) state
      (visited (visited-key state)) (recur t visited (inc i))
      :else
      (recur
       (concat t (next-states state visited))
       (conj visited (visited-key state))
       (inc i)
       ))))

(bfs 1
     (fn [x] [(inc x) (dec x)]) #{10} identity)


(time
    (-> (bfs
         4000
         {:intcode-state (intcode/init-state program)
          :pos [0 0]
          :grid {}
          :path []}
         (fn [s v]
           (let [new-states (for [d (not-back (:dir s) )
;                                  :when (not (v (move (:pos s) d)))
                                  :let [ns (step s d)]
                                 #_#_ :when (not= hit-wall (:result ns))]
                              ns)
                 g
                 (apply merge (:grid s) (map :grid new-states))]
             (for [ns new-states
                   :when (not= hit-wall (:result s))]
               (assoc ns :grid g)
               )

             ))
         (comp #{2} :result)
         (comp vec (juxt :pos)))
;        :exhaust
        :visited
                                               count
;        show
        ))


(def img (img/new-image 200 200))

(defn pos->img [[x y]]
  [(+ x (/ (img/width img) 2))
   (+ y (/ (img/height img) 2) )])

(defn show [{:keys [dir grid] :as state}]
  (dotimes [x (img/width img)]
    (dotimes [y (img/height img)]
      (img/set-pixel img x y 0xFF000000)))
  (doseq [ [pos tile] grid]
     (let [[ix iy] (pos->img pos)]
      (img/set-pixel img ix iy (case tile
                                 :wall 0xFFFFFFFF
                                 :empty 0xFF00FF88
                                 :oxygen-system 0xFFFF0000))))



  (img/show img :resize 4)

  (select-keys state [:dir :pos :result])
  )

(defn showv [visited]
  (dotimes [x (img/width img)]
    (dotimes [y (img/height img)]
      (img/set-pixel img x y 0xFF000000)))
  (doseq [ pos  visited]
     (let [[ix iy] (pos->img pos)]
      (img/set-pixel img ix iy 0xFFFFFFFF)))



  (img/show img :resize 4)
  (count visited)
  )



(show (reduce step init-state (repeat 100 south)))

(defn step-program [intcode-state input]
  )
