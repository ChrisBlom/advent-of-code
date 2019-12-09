(ns adventofcode.2019.day05
  (:require [midje.sweet :refer :all :except any?]
            [clojure.java.io :as io]))

(defn parse [input]
  (->> input
       (format "[%s]")
       read-string))

(def input
  (->> "2019/day05.txt"
       io/resource
       slurp
       parse))

(defn parse-parameter-mode [p]
  {:ma (quot p 100)
   :mb (quot (rem p 100) 10)
   :mc (rem  (rem p 100) 10)})

(fact
  (parse-parameter-mode 987)
  => {:ma 9 :mb 8 :mc 7}

  (parse-parameter-mode 12)
  => {:ma 0 :mb 1 :mc 2}

  (parse-parameter-mode 3)
  => {:ma 0, :mb 0, :mc 3})

(defn parse-opcode [opcode]
  (assoc (parse-parameter-mode (quot opcode 100))
         :op (rem opcode 100)))

(fact
  (parse-opcode 1002)
  =>
  {:ma 0, :mb 1, :mc 0, :op 2}

)

(defn memload [buffer parameter mode relative-base]
  (case mode
    ;; position mode
    0 (do
        (assert (contains? buffer parameter))
        (get buffer parameter))
    ;; immediate mode
    1  parameter
    ;; relative mode
     (do
        (assert (contains? buffer parameter))
        (get buffer (+ parameter )))
    ))


(defn new-state [instruction-pointer relative-base buffer in out]
  {:instruction-pointer instruction-pointer
   :relative-base relative-base
   :buffer buffer
   :in in
   :out out})

(defn init-state
  [intcode input]
  {:pre [(sequential? input)]}
  (new-state 0 0 intcode (apply list input) []))

(defn- step-read
  [{:keys [in] :as state} p1]
  (assert (list? in))
  (-> state
      (update :instruction-pointer + 2)
      (assoc-in [:buffer p1] (first in))
      (update :in rest)))

(defn step-intcode
  [{:keys [instruction-pointer relative-base buffer in out]
    :as state}]
  {:pre [(contains? buffer instruction-pointer)
         (list? in)
         (vector? out)]}
  (let [[opcode p1 p2 p3 :as instr] (subvec buffer instruction-pointer)
        {:keys [op ma mb mc]} (parse-opcode opcode)
        param1 (fn [] (memload buffer p1 mc relative-base))
        param2 (fn [] (memload buffer p2 mb relative-base))
        memset (fn [address value]
                 (assoc buffer address value))]
    (case op
      ;; +
      1 (new-state (+ 4 instruction-pointer)
                   relative-base
                   (memset p3 (+ (param1) (param2)))
                   in
                   out)
      ;; *
      2 (new-state (+ 4 instruction-pointer)
                   relative-base
                   (memset p3 (* (param1) (param2)))
                   in
                   out)
      ;; read
      3 (do (assert (seq in) "Cannot read from empty input")
            (step-read state p1))
      ;; write
      4 (do #_(println "WRITE: " (param1))
            (-> state
                (update :instruction-pointer + 2)
                (update :out conj (param1))))
      ;; jump if true
      5 (assoc state :instruction-pointer
               (if (not (zero? (param1)))
                 (param2)
                 (+ 3 instruction-pointer)))

      ;; jump if false
      6 (assoc state :instruction-pointer
               (if (zero? (param1))
                 (param2)
                 (+ 3 instruction-pointer)))
      ;; less than
      7 (new-state (+ 4 instruction-pointer)
                   relative-base
                   (memset p3 (if (< (param1) (param2)) 1 0))
                   in
                   out)
      ;; equals
      8 (new-state (+ 4 instruction-pointer)
                   relative-base
                   (memset p3 (if (= (param1) (param2)) 1 0))
                   in
                   out)
      ;; halt
      99 (assoc state :halted true)

      (throw (ex-info "invalid opcode" {:op op})))))

(defn halted? [{:keys [halted]}]
  halted)

(defn run-intcode- [state & {:keys [halt?] :or {halt? halted?}}]
  (->> (iterate step-intcode state)
       (drop-while (complement halt?))
       first))

(defn run-intcode [intcode input]
  (run-intcode- (init-state intcode input)))

(fact "io tests"
  (select-keys (run-intcode [ 1101,100,-1,4,0] [1]) [:buffer :out])
  =>
  {:buffer [1101 100 -1 4 99], :out []}

  (select-keys (run-intcode [3,0,4,0,99] ["hello world"]) [:buffer :out])
  => {:buffer ["hello world" 0 4 0 99], :out ["hello world"]}


  (:out (run-intcode input [1]))
  =>
  [0 0 0 0 0 0 0 0 0 13933662]

  )

(fact "jump tests"
  (:out (run-intcode [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0]))
  => [0]

  (:out (run-intcode [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [2]))
  => [1]

  (:out (run-intcode [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [0]))
  => [0]

  (:out (run-intcode [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [2]))
  => [1]

  (tabular

   (fact (:out (run-intcode [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                             1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                             999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
                            ?in)) => ?out)

   ?in ?out
   [7] [999]
   [8] [1000]
   [9] [1001]))

(fact "part 1"
  (last (:out (run-intcode input [1])))
  =>
  13933662)

(fact "part 2"
  (first (:out (run-intcode input [5])))
  => 2369720)
