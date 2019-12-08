(ns adventofcode.2019.day07
  (:require
   [adventofcode.2019.day05 :as day5]
   [clojure.java.io :as io]
   [midje.sweet :refer :all :except any?]))

(defn parse [x]
  (-> (format "[%s]" x)
      read-string))

(def input-intcode
  (->> "2019/day07.txt"
       io/resource
       slurp
       parse))

(defn phase-setting? [x]
  (<= 0 x 4))

(defn output-signal
  [intcode phase-setting input-signal]
  {:pre (phase-setting? phase-setting)}
  (->> [phase-setting input-signal]
       (day5/run-intcode intcode)
       :out
       peek))

(defn permutations [xs]
  {:pre [(set? xs)]}
  (if (seq (rest xs))
    (for [o xs
          p (permutations (disj xs o))]
      (cons o p))
    (list xs)))

(fact
  (sort-by vec (permutations #{1 2 3}))
  =>
  [[1 2 3] [1 3 2]
   [2 1 3] [2 3 1]
   [3 1 2] [3 2 1]])

(defn run-sequence [intcode sequence]
  (let [initial-signal 0]
    (reduce
     (fn [signal phase-setting]
       (output-signal intcode phase-setting signal))
     initial-signal
     sequence)))

(def phase-settings-p1 [0 1 2 3 4])

(defn find-max-output-p1 [intcode phase-settings]
  (->> (for [sequence (permutations (set phase-settings))]
         {:sequence sequence
          :signal (run-sequence intcode sequence)})
       (apply max-key :signal)))

(fact
  (find-max-output-p1 (parse  "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
                      phase-settings-p1)
  =>
  {:sequence '(4 3 2 1 0), :signal 43210}

  (find-max-output-p1 (parse  "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
                      phase-settings-p1)
  => {:sequence '(0 1 2 3 4), :signal 54321}

  (find-max-output-p1 (parse  "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
                      phase-settings-p1
                      )
  {:sequence '(1 0 4 3 2), :signal 65210})

(fact "part 1"
  (find-max-output-p1 input-intcode phase-settings-p1)
  =>
  {:sequence '(1 0 3 4 2), :signal 21760})

(defn step-feedback
  [{:keys [signal states-by-phase-setting]}
   phase-setting]
  (let [prev-state (get states-by-phase-setting phase-setting)
        inputs (if (:initial prev-state)
                 (list phase-setting signal)
                 (list signal))
        halt? (fn [x]
                (or (day5/halted? x)
                    (> (count (:out x))
                       (count (:out prev-state)))))
        new-state (day5/run-intcode-  (assoc prev-state :in inputs)
                                      :halt? halt?)
        new-signal (peek (:out new-state))
        new-states (assoc states-by-phase-setting phase-setting new-state)
        all-halted (every? day5/halted? (vals new-states))]
    (if all-halted
      (reduced (-> new-state :out peek))
      {:signal new-signal
       :states-by-phase-setting new-states})))

(defn run-sequence-feedback [intcode sequence]
  (let [initial-signal 0
        initial-states
        (into {}
              (for [phase-setting sequence]
                [phase-setting (assoc (day5/init-state intcode [phase-setting initial-signal])
                                      :initial true)]))]
    (reduce step-feedback
            {:signal 0
             :states-by-phase-setting initial-states}
            (cycle sequence))))

(fact "part 2 examples"
  (let [intcode [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5]]
    (run-sequence-feedback intcode [9,8,7,6,5]))
  =>
  139629729

  (let [intcode [3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54 -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4 53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10]]
    (run-sequence-feedback intcode [9,7,8,5,6]))
  =>
  18216)

(def phase-settings-p2 [9 8 7 6 5])

(defn find-max-output-p2 [intcode phase-settings]
  (->> (for [sequence (permutations (set phase-settings))]
         {:sequence sequence
          :signal (run-sequence-feedback intcode sequence)})
       (apply max-key :signal)))

(fact
  (find-max-output-p2  (parse "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
                       phase-settings-p2)
  =>
  {:sequence '(9 8 7 6 5), :signal 139629729})

(fact "part 2"
  (find-max-output-p2
   input-intcode
   phase-settings-p2)
  =>
  {:sequence '(8 9 5 6 7), :signal 69816958})
