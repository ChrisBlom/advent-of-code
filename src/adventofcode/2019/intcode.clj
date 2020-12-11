(ns adventofcode.2019.intcode
  (:require [midje.sweet :refer :all :except any?]))

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
  => {:ma 0, :mb 0, :mc 3}

  (parse-parameter-mode 200)
  => {:ma 0, :mb 0, :mc 3}
  )

(defn parse-opcode [opcode]
  (assoc (parse-parameter-mode (quot opcode 100))
         :op (rem opcode 100)))

(fact
  (parse-opcode 1002)
  =>
  {:ma 0, :mb 1, :mc 0, :op 2})

(defn memload [buffer parameter mode relative-base]
  (case mode
    ;; position mode
    0 (do
        (assert (not (neg-int? parameter)))
        (get buffer parameter 0))
    ;; immediate mode
    1  parameter
    ;; relative mode
    2 (let [adress (+ parameter relative-base)]
        (assert (not (neg-int? adress)))
        (get buffer adress 0))))

(defn memstore [buffer parameter mode relative-base value]
  (let [address (case mode
                  ;; position mode
                  0 parameter
                  ;; relative mode
                  2 (+ parameter relative-base))]
    (assert (not (neg-int? address)))
    (assoc buffer address value)))

(defn new-state [instruction-pointer relative-base buffer in out]
  {:instruction-pointer instruction-pointer
   :relative-base relative-base
   :buffer buffer
   :in in
   :out out})

(defn buffer->vec [buffer]
  (let [high  (apply max-key identity (keys buffer)) ]
    (mapv buffer (range 0 (inc high)))))

(defn init-state
  ([intcode]
   (init-state intcode (list)))
  ([intcode input]
   {:pre [(sequential? input)]}
   (let [buffer (apply hash-map (mapcat list (range) intcode))
         in (apply list input)]
     (new-state 0 0 buffer in []))))

(defn- step-read
  [{:keys [in relative-base] :as state} p1 ma]
  (assert (seq? in))
  (-> state
      (update :instruction-pointer + 2)
      (update :buffer memstore p1 ma relative-base (first in))
      (update :in rest)))

(defn step-intcode
  [{:keys [instruction-pointer relative-base buffer in out await-input]
    :as state}]
  {:pre [(contains? buffer instruction-pointer)
         (seq? in)
         (vector? out)]}
  (let [state (cond-> state
                (seq in) (dissoc :await-input))
        [opcode p1 p2 p3 :as instr] (map buffer (iterate inc instruction-pointer))
        {:keys [op ma mb mc]} (parse-opcode opcode)
        param1 (fn [] (memload buffer p1 mc relative-base))
        param2 (fn [] (memload buffer p2 mb relative-base))
        memset (fn [address mode value]
                 (memstore buffer address mode relative-base value))]
    (case op
      ;; +
      1 (new-state (+ 4 instruction-pointer)
                   relative-base
                   (memset p3 ma (+ (param1) (param2)))
                   in
                   out)
      ;; *
      2 (new-state (+ 4 instruction-pointer)
                   relative-base
                   (memset p3 ma (* (param1) (param2)))
                   in
                   out)
      ;; read
      3 #_(do (assert (seq in) "Cannot read from empty input")
              (step-read state p1 mc))
      (if (empty? in)
        (assoc state :await-input true)
        (dissoc (step-read state p1 mc) :await-input))
      ;; writ
      4 (-> state
            (update :instruction-pointer + 2)
            (update :out conj (param1)))
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
                   (memset p3 ma (if (< (param1) (param2)) 1 0))
                   in
                   out)
      ;; equals
      8 (new-state (+ 4 instruction-pointer)
                   relative-base
                   (memset p3 ma (if (= (param1) (param2)) 1 0))
                   in
                   out)
      ;; adjust relative base
      9 (-> state
            (update :instruction-pointer + 2)
            (update :relative-base + (param1)))
      ;; halt
      99 (assoc state :halted true)

      (throw (ex-info "invalid opcode" {:op op})))))

(defn halted? [{:keys [halted]}]
  halted)

(defn has-output? [{:keys [out]}]
  (seq out))

(defn run-intcode- [state & {:keys [halt?] :or {halt? halted?}}]
  (->> (iterate step-intcode state)
       (drop-while (complement halt?))
       first))

(defn run-intcode-io [state & input]
  (let [{:keys [out halted] :as new-state}
        (->> (iterate step-intcode (assoc state :in (apply list input) :out []))
             (drop-while (complement
                          (some-fn halted?
                                   has-output?)
                          ))
             first)]
    new-state
    ))

(defn run-intcode [intcode input]
  (run-intcode- (init-state intcode input)))

(fact "io tests"

  (let [{:keys [buffer out]} (run-intcode [ 1101,100,-1,4,0] [1])]

    (buffer->vec buffer)
    => [1101 100 -1 4 99]

    out
    =>  []
    )

  (fact
    (let [{:keys [buffer out]} (run-intcode [3,0,4,0,99] ["hello world"])]

      (buffer->vec buffer)
      => ["hello world" 0 4 0 99]

      out
      => ["hello world"]))



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


(fact "day 9 part 1 examples"
  (let [quine [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]]
    (:out (run-intcode quine []))
    => quine)

  (first (:out (run-intcode [1102,34915192,34915192,7,4,7,99,0] [])))
  => #(-> % str count #{16})


  (let [large-num 1125899906842624]
    (first (:out (run-intcode [104,large-num,99] [])))
    => large-num)

  (:out (run-intcode [109,12,203,-5,204,-5,99] [1]))
  => [1])
