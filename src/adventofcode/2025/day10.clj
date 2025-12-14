(ns adventofcode.2025.day10
  (:require
   [clojure.math :as m]
   [adventofcode.utils :as u]
   [clojure.string :as str]))

(def ex
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(defn parse-line [l]
  (let [[_ lights wiring joltages]
        (re-matches #"\[(.+)\] (.*) \{(.*)\}" l)]

    {:target-lights (vec lights)
     :button-wiring (read-string (format "[%s]" wiring))
     :target-joltages (read-string (format "[%s]" joltages))}))

(defn parse [x]
  (map-indexed (fn [i l] (assoc (parse-line l) :idx i))
               (str/split-lines x)))

(defn toggle-light [x]
  (case x
    \. \#
    \# \.))

(defn press [state button-wiring]
  (reduce
   (fn [acc n]
     (update acc n toggle-light))
   state
   button-wiring))

(assert (= (press (vec "...")
                   '(0 1)
                   )
           [\# \# \.]))

(defn min-button-presses [{:keys [target-lights button-wiring]}]
  (let [state-key (fn [state] (select-keys state [:lights :button-wiring-unused]))]
    (loop [ todo [{:lights (vec (repeat (count target-lights) \.))
                   :button-wiring-unused (zipmap (range) button-wiring)
                   :steps 0}]
           ;; state-key -> shortest steps in which this state is reachable
           shortest {}
           ]
      (if-some [ h (first todo)]
        (cond
          ;; bfs ensures that if we reach the target, we took the min steps:
          (= target-lights (:lights h ))
          (:steps h)

          ;; dead end
          (empty? (:button-wiring-unused h))
          (recur (rest todo) shortest)

          ;; prune paths that are not better
          (>= (:steps h)
              (get shortest (state-key h) Long/MAX_VALUE))
          (recur (rest todo) shortest)


          :else
          (recur
           (concat (rest todo)
                   (for [ [i b] (:button-wiring-unused h)]
                     (-> h
                         ;; no use in pressing a button twice:
                         (update :button-wiring-unused dissoc i)
                         (update :lights press b)
                         (update :steps inc))))
           ;; track shortest path by state
           (assoc shortest
                  (state-key h)
                  (:steps h))))
        :err))))

(defn part-1 [in]
  (reduce + (pmap min-button-presses (parse in))))

(assert (= 7 (part-1 ex)))

(defn press-2 [state button-wiring]
  (reduce
   (fn [acc n]
     (update acc n inc))
   state
   button-wiring))

(press-2 [0 0] '(0 1))
(press-2 (press-2 [0 0] '(0 1)) '(1))

(defn min-button-presses-2 [{:keys [button-wiring target-joltages]}]
  (let [state-key (fn [state] (:joltages state))]
    (time
     (loop [ todo [{:joltages (vec (repeat (count target-joltages) 0))
                    :presses 0}]
            ;; state-key -> shortest steps in which this state is reachable
            shortest {}]
       (if-some [ {:keys [joltages presses] :as h} (first todo)]
         (do #_(println h (count todo))
             (cond
               ;; prune paths that are not better
               (>= (:presses h)
                   (get shortest (state-key h) Long/MAX_VALUE))
               (recur (rest todo) shortest)

               :else
               (recur
                (concat (for [ b button-wiring
                              :let [new-joltages (press-2 joltages b)]
                              :when (not (some true? (map > (:joltages h) target-joltages)))
                              ]
                          {:joltages new-joltages
                           :presses (inc presses)})
                        (rest todo))
                ;; track shortest path by state
                (update shortest (state-key h) (fn [x] (if x (min x presses) presses))))))
         (shortest target-joltages))))))



(defn wiring-vecs [{:keys [target-joltages button-wiring]}]
  (let [ i (vec (repeat (count target-joltages) 0)) ]
    (for [ b button-wiring]
      (press-2 i b))))

(require '[clojure.java.shell :as sh])

(defn to-minizinc-json [{:keys [target-joltages button-wiring] :as x}]
  {:C (count target-joltages)
   :R (count button-wiring)
   :M (apply concat (wiring-vecs x))
   :x target-joltages})

(comment
  (to-minizinc-json (first (parse ex))))

(require '[clojure.data.json :as json])

(clojure.java.io/make-parents
   (str "src/adventofcode/2025/tmp/.keep" ))

(defn solve-minizinc [{:keys [idx target-joltages button-wiring] :as x}]
  (spit (str "src/adventofcode/2025/tmp/" idx ".json")
        (json/write-str (to-minizinc-json x)))
  (let [res (sh/sh "minizinc"
                   "--solver"
                   "COIN-BC"
                   "day10.mzn"
                   (str "tmp/" idx ".json")
                   :dir
                   "src/adventofcode/2025")]
    (assert (zero? (:exit res)))
    (reduce + (read-string (first (str/split-lines (:out res)))))))


(comment

  (map (juxt
        min-button-presses-2
        solve-minizinc)
       (parse ex))

    )

(defn part-2 [in]
  (reduce + (pmap solve-minizinc (parse in))))

{:part-1 (part-1 (user/day-input))
 :part-2 (part-2 (user/day-input))}
