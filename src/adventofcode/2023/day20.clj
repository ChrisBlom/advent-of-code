(ns adventofcode.2023.day20
  (:require [clojure.string :as str]))

(def ex-1 "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(defn parse-line [l]
  (let [ [src trgs ] (clojure.string/split l #" -> ")
        trgs (mapv keyword (clojure.string/split trgs #", "))

        type (cond
               (str/starts-with? src "&") :conjunction
               (str/starts-with? src "%") :flip-flop
               :else :broadcaster)

        id (keyword (if (= :broadcaster type) src (subs src 1)))
        ]
    [id
     {:type type
      :trgs trgs}]))

(defn parse- [s]
  (into { }(map parse-line (clojure.string/split-lines s))))

(parse- ex-1)

(defn add-sources [g]
  (reduce-kv
   (fn [acc src v]
     (reduce (fn [acc- trg]
               (update-in acc- [trg :srcs] (fnil conj #{}) src ))
             acc
             (:trgs v)))
   g
   g))

(defn add-states [g]
  (update-vals g (fn [v]
                   (case (:type v)
                     :conjunction (assoc v :id->last-signal (zipmap (:srcs v) (repeat :low)))
                     :flip-flop (assoc v :state :off)
                     v))))

(defn parse [input]
  (-> input
      parse-
      add-sources
      add-states))

(def flip
  {:low :high
   :high :low})

(defn update-node [g src-id id signal]
  (let [{:keys [state type trgs srcs id->last-signal] :as n} (g id)
        [next-n signal-out]
        (case type
          nil [n signal]
          :broadcaster [n signal]
          :flip-flop (let [ [nstate signal-out]  (case [state signal]
                                                   [:off :low] [:on :high]
                                                   [:on :low] [:off :low]
                                                   [state nil])]
                       [(assoc n :state nstate)
                        signal-out])
          :conjunction
          (let [id->last-signal-after (assoc id->last-signal src-id signal)
                all-high (apply = :high (vals id->last-signal-after))]
            [(assoc n :id->last-signal id->last-signal-after)
             (if all-high :low :high)])

          (throw (ex-info "unknown type" n))
          )

        ]
    [(assoc g id next-n)
     (when signal-out
       (mapv vector
             (repeat id)
             (repeat signal-out)
             trgs))]))

(update-node (parse ex-1) nil :broadcaster :low)

(defn step [g  signals]
  (reduce (fn [ [g- out] [src-id signal trg-id] ]
            (let [ [g-- out-step] (update-node g- src-id trg-id signal)]
              [g-- (concat out out-step)]))
          [g []]
          signals))

(step (parse ex-1) [[nil :low :broadcaster]])

(defn simulate-
  [{:keys [state]}]
  (loop [g state
         signals [[:button :low :broadcaster]]
         sent-signals []]
    (let [[g- signals-] (step g signals)]
      (if (seq signals)
        (recur g-
               signals-
               (concat sent-signals signals))
        {:state g-
         :signals (concat sent-signals signals)}))))

(def ex-2
  "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")


(take 3
      (rest
       (iterate simulate-
                {:state (parse ex-2)})))


(defn part-1 [input]
  (let [signals (map :signals
                     (take 1000
                           (rest
                            (iterate simulate-
                                     {:state (parse input)}))))
        freqs (frequencies (map second (apply concat signals)))]
    (apply * (vals freqs))))

(defn find-conjunction [g id]
  (let [{:keys [type srcs]} (g id)]
    (if (and (= type :conjunction)
             (> (count srcs) 1))
      #{id}
      (apply clojure.set/intersection
             (map #(find-conjunction g %) srcs)))))

(defn downsteam [g t]
  (loop [res {}
         todo [t]
         visited #{}]
    (if-some [id (first todo)]
      (if (visited id)
        (recur res (rest todo) visited)
        (let [n (g id)]
          (recur
           (assoc res id n)
           (concat (:srcs n) (rest todo))
           (conj visited id))))
      res)))

(defn partition-graph [id g]
  (let [c (first (find-conjunction (parse (user/day-input))
                                   id))
        _ (assert c "not found")]
    (map #(downsteam g %) (:srcs (g c)))))


(defn sends? [output-signals volt target]
  (first (filter  (fn [ [from v t]] (and  (= v volt)
                                         (= t target))) output-signals)))


(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

(defn lcm [a b]
  (* (abs b) (/ (abs a)
                (gcd a b))))


(defn part-2 [input]
  (->>
   input
   parse
   (partition-graph :rx)
   (map (fn [pg]
          (->> (map list
                    (range)
                    (iterate simulate-
                             {:state pg}))
               rest
               (drop-while #(not (sends? (:signals (second %)) :high :ll)))
               (ffirst)))
        )
   (reduce lcm)) )


(require '[clojure.java.shell :as sh])

(require '[rhizome.viz])


(defn view [g]
  (rhizome.viz/view-image
   (rhizome.viz/dot->image
    (with-out-str
      (println "digraph {")
      (doseq [ [id {:keys [type trgs]}] g]

        (let [label (str (case type
                           :conjunction "&"
                           :flip-flop "%"
                           "")
                         (name id)
                         )]
          (println (name id) (str "[ label = \"" label "\" ]")))

        (doseq [t trgs]
          #_(println (str (name id) ":s" ) " -> " (str (name t) ":n"))
          (println (str (name id) ) " -> " (str (name t) )))
        )
      (println "}")))))

(comment

  (view (parse (user/day-input)))

  (run! view (partition-graph :rx (parse (user/day-input))))

  )


{:part-1 (part-1 (user/day-input))
 :part-2 (part-2 (user/day-input))}
