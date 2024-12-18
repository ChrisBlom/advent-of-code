>(ns adventofcode.2024.day17
  (:require
   [adventofcode.utils :as u]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0")

(defn parse-nums [s]
  (map parse-long (str/split (second (str/split s #": ")) #",")))

(defn parse [x]
  (let [ [ra rb rc _ pr ] (str/split-lines x)]
    {:ra (first (parse-nums ra))
     :rb (first (parse-nums rb))
     :rc (first (parse-nums rc))
     :program (vec (parse-nums pr))
     :i 0
     :o []}))

(defn halt! [state]
  (throw (ex-info "halt" (assoc state :halt true))))

(parse ex)

(defn co [state n]
  (case n
    nil (halt! state)
    (0 1 2 3) n
    4 (:ra state)
    5 (:rb state)
    6 (:rc state)
    7 (throw (ex-info (format "unexpected input %s" n) state))
    (throw (ex-info (format "illegal co op %s" n) state))))

(def hist (atom []))

(defn step [{:keys [ra rb rc program i] :as s}]
 #_ (swap! hist conj s)
  (let [instr (get program i)
        lit-op (get program (inc i))]
    (case instr
      nil (halt! s)
      ;; adv
      0 (assoc s
               :ra (long  (/ ra
                             (math/pow 2 (co s lit-op))))
               :i (+ i 2))
      ;; bxl
      1 (assoc s
               :rb (bit-xor rb lit-op)
               :i (+ i 2))
      ;; bst
      2 (assoc s
               :rb (mod (co s lit-op) 8)
               :i (+ i 2))
      ;; jnz
      3 (if (= ra 0)
          (assoc s :i (+ i 2))
          (assoc s :i lit-op))
      ;; bxc
      4 (assoc s
               :rb (bit-xor rb rc)
               :i (+ i 2))
      ;; out
      5 (assoc s
               :i (+ i 2)
               :out (conj (:out s []) (mod (co s lit-op) 8) ))
      ;; bdv
      6 (assoc s
               :rb (long  (/ ra (math/pow 2 (co s lit-op))))
               :i (+ i 2))
      ;; cdv
      7 (assoc s
               :rc (long (/ ra (math/pow 2 (co s lit-op))))
               :i (+ i 2)
               ))))

@hist

(run (parse
      (user/day-input)))

(step (parse ex))

(defn run [p]
  (swap! hist empty)
  (try
    (loop [s p
           seen #{}
           rem 100000]
      (let [id (select-keys p [:ra :rb :rc :i])]
        (cond
          (zero? rem)
          (assoc s :error :too-many-iters)

          (:halt s)
          s

          (:seen id)
          (assoc s :loop true)

          :else
          (recur (step s)
                 (conj seen id)
                 (dec rem))))

      )
    (catch clojure.lang.ExceptionInfo e
      (if (:halt (ex-data e))
        (ex-data e)
        (throw e)))))

  (assert (= 1 (:rb (step (parse  "Register A: 0
Register B: 0
Register C: 9

Program: 2,6")))))


(assert (= [0 1 2]
           (:out (run (parse  "Register A: 10
Register B: 0
Register C: 0

Program: 5,0,5,1,5,4")))))


(assert (=
         {:out [4,2,5,6,7,7,7,7,3,1,0] :ra 0}
         (select-keys  (run (parse  "Register A: 2024
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"))
                       [:out :ra])))

(assert (= 26 (:rb (run (parse  "Register A: 0))
Register B: 29
Register C: 0

Program: 1,7")))))


(assert (= 44354
           (:rb (run (parse  "Register A: 0
Register B: 2024
Register C: 43690

Program: 4,0")))))



(defn part-1 [input]
  (let [x (parse input)
        rx (run x)]
    (str/join "," (:out  rx))))

(assert (= "4,6,3,5,6,3,5,2,1,0" (part-1 ex)))

(run
    (assoc (parse (user/day-input))
           :ra 1
           ))

(comment
  (part-1 (user/day-input))




  (take 1000   @hist)

  )

(defn part-2 [input]
  (let [x (parse input)]))

  (assert (= x (part-2 ex)))

  {:part-1 (part-1 (user/day-input))
   :part-2 (time (part-2 (user/day-input)))}


  (comment



    )


(def ex-2 "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0")


(run (assoc (parse ex-2) :ra 117440))


(mod 117440 8)

(/ (math/log )
   (math/log ))

(/ 117440 2 2 2 2 2 2 )

(first
 (filter
  #(str/starts-with? "2,4,1,4" %)
  (map (fn [i]
         (str/join "," (:out (run (assoc (parse (user/day-input)) :ra

                                         (* i  2 2 2 2 2 2))))))
       (range 0 1000000)
       )
  ))

*1

@hist
