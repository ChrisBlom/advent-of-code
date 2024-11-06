(ns adventofcode.2023.day25
  (:require [clojure.string :as str]))

(def ex
  "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr")


(defn parse-line [s]
  (let [[ src _ & trgs ] (str/split s #"[: +]")]
    (for [t trgs]

      #{src t}
      )))


(defn parse [input ]
  (->> input
       str/split-lines
       (mapcat parse-line)
       set))




(defn viz [g]
  (spit "out.dot"
        (with-out-str
          (println "digraph foo { ")
          (doseq [n (nodes g)]
            (println n "[label=\"" n "\",shape=\"plaintext\"] ")
            )
          (doseq [e g]
            (println (first e) "->" (second e) "[dir=both]")

            )
          (println "}")))
#_  (apply clojure.java.shell/sh (str/split "neato -x   -Grepulsiveforce=30   -Tsvg -O out.dot" #" +")))
                                        ;  "out.dot.svg"

(viz (parse ex))
(viz
 (disj (parse ex)
       #{"pzl" "hfx"}
       #{"nvd" "jqt"}
       #{"cmg" "bvb"}))


(defn neighbours [g]
  (reduce
   (fn [acc edge]
     (-> acc
         (update (first edge) (fnil conj #{}) (second edge))
         (update (second edge) (fnil conj #{}) (first edge))
         ))
   {}
   g))


(defn nodes [g]
  (reduce set/union g))

(require '[clojure.set :as set])

(defn count-components [g]
  (let [n (neighbours g)]
    (loop [ todo #{(first (nodes g))}
           visited #{}]
      (if-let [c (first todo)]
        (if (visited c)
          (recur (rest todo) visited)
          (recur (set/union todo (set/difference (get n c) visited))
                 (conj visited c)))
        (* (count visited)
           (- (count (nodes g)) (count visited)))


        ))))

(count-components
 (disj (parse ex)
       #{"pzl" "hfx"}
       #{"nvd" "jqt"}
       #{"cmg" "bvb"}))


(viz (parse  (user/day-input)))

(count-components
 (disj (parse (user/day-input))
       #{"xzn" "dsr"}
       #{"tbq" "qfj"}
       #{"xbl" "qqh"}))




(def edges-d2
  [#{"xzn" "dsr"}
   #{"tbg" "qfj"}
   #{"xbl" "qqh"}])

(def n (->> #_(user/day-input)
            ex
            str/split-lines
            (mapcat parse-line)
            )


  )
