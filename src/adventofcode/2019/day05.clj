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
  (let []
    {:ma (quot p 100)
     :mb (quot (mod p 100) 10)
     :mc (rem  (mod p 100) 10)}))

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
  {:ma 0, :mb 1, :mc 0, :op 2})

(defn memload [buffer parameter mode]
  (case mode
    ;; position mode
    0 (get buffer parameter)
    ;; immediate mode
    1  parameter))

(defn run-intcode
  ([intcode input]
   (loop [ instruction-pointer 0
          buffer intcode
          in (apply list input)
          out []]
     (let [[opcode p1 p2 p3] (subvec buffer instruction-pointer)
           {:keys [op ma mb mc]} (parse-opcode opcode)
           param1 (fn [] (memload buffer p1 mc))
           param2 (fn [] (memload buffer p2 mb))
           memset (fn [address value] (assoc buffer address value))]
       (case op
         ;; +
         1 (recur (+ 4 instruction-pointer)
                  (memset p3 (+ (param1) (param2)))
                  in
                  out)
         ;; *
         2 (recur (+ 4 instruction-pointer)
                  (memset p3 (* (param1) (param2)))
                  in
                  out)
         ;; read
         3 (recur (+ 2 instruction-pointer)
                  (memset p1 (first in))
                  (rest in)
                  out)
         ;; write
         4 (recur (+ 2 instruction-pointer)
                  buffer
                  in
                  (conj out (param1)))
         ;; jump if true
         5 (recur (if (not (zero? (param1)))
                    (param2)
                    (+ 3 instruction-pointer))
                  buffer
                  in
                  out)
         ;; jump if false
         6 (recur (if (zero? (param1))
                    (param2)
                    (+ 3 instruction-pointer))
                  buffer
                  in
                  out)
         ;; less than
         7(recur (+ 4 instruction-pointer)
                 (memset p3 (if (< (param1) (param2)) 1 0))
                  in
                  out)
         ;; equals
         8 (recur (+ 4 instruction-pointer)
                  (memset p3 (if (= (param1) (param2)) 1 0))
                  in
                  out)

         ;; halt
         99 {:buffer buffer
             :output out})))))

(fact "io tests"
  (run-intcode [ 1101,100,-1,4,0] [1])
  =>
  {:buffer [1101 100 -1 4 99], :output []}

  (run-intcode [3,0,4,0,99] ["hello world"])
  => {:buffer ["hello world" 0 4 0 99], :output ["hello world"]}


  (:output (run-intcode input [1]))
  =>
  [0 0 0 0 0 0 0 0 0 13933662]

  )

(fact "jump tests"
  (:output (run-intcode [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0]))
  => [0]

  (:output (run-intcode [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [2]))
  => [1]

  (:output (run-intcode [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [0]))
  => [0]

  (:output (run-intcode [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [2]))
  => [1]

  (tabular

   (fact (:output (run-intcode [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
                               ?in)) => ?out)

   ?in ?out
   [7] [999]
   [8] [1000]
   [9] [1001]))

(fact "part 1"
  (last (:output (run-intcode input [1])))
  =>
  13933662)

(fact "part 2"
  (first (:output (run-intcode input [5])))
  => 2369720)
