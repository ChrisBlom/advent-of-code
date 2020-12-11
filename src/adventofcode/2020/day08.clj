(ns adventofcode.2020.day08
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def example
"nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")


(defn parse-line [line]
  (let [ [op arg] (str/split line #" ")]
    {:op op
     :arg (read-string arg)}))


(map parse-line (str/split-lines example))


(defn apply-instruction
  [{:keys [accumulator  instruction-pointer ] :as state}
   {:keys [op arg]}]

  (case op
    "acc" (-> state
              (update :instruction-pointer inc)
              (update :accumulator + arg))
    "jmp" (update state :instruction-pointer + arg)
    "nop" (update state :instruction-pointer inc)))


(apply-instruction {:accumulator 0 :instruction-pointer 0} {:op "acc" :arg 20})

(defn execute [program]
  (loop [{:keys [instruction-pointer accumulator] :as state}
         {:accumulator 0
          :instruction-pointer 0}
         visited #{}
         limit 10000]
    (when (> limit 0)
      (if (visited instruction-pointer)
        accumulator
        (let [instruction (program instruction-pointer)
              next-state  (apply-instruction state instruction)]
          (recur next-state
                 (conj visited instruction-pointer)
                 (dec limit)))))))

(assert (= 5 (execute (mapv parse-line (str/split-lines example)))))

(def input
  (mapv parse-line (str/split-lines (slurp (io/resource "2020/day08.txt")))))

;; part 1

(defn execute-2 [program]
  (loop [{:keys [instruction-pointer accumulator] :as state}
         {:accumulator 0
          :instruction-pointer 0}
         limit 100000]
    (when (> limit 0)
      (if (= instruction-pointer (count program))
        accumulator
        (let [instruction (program instruction-pointer)
              next-state  (apply-instruction state instruction)]
          (recur next-state
                 (dec limit)))))))

(defn correct-instruction [{:keys [op arg] :as instr}]
  (case op
    "jmp" {:op "nop" :arg arg}
    "nop" {:op "jmp" :arg arg}
    nil))

(def part-2-example
  (mapv parse-line (str/split-lines "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")))

(defn variations [program]
  (for [i (range 0 (count program))
        :let [corrected (correct-instruction (program i))]
        :when corrected]
    (assoc program i corrected)))


(let [result (promise)]
  (doseq [v (variations part-2-example)]
    (future (deliver result (execute-2 v))))
  @result)

{:part-1 (execute input)
 :part-2 (let [result (promise)]
           (doseq [v (variations input)]
             (future (deliver result (execute-2 v))))
           @result
           )}
