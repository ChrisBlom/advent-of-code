(ns adventofcode.2025.day11
  (:require
   [clojure.math :as m]
   [adventofcode.utils :as u]
   [clojure.string :as str]))

(def ex
  "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(def ex-2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

(defn parse-line [l]
  (let [ [src & trgs ] (str/split l #":?\s+")]
    [(keyword src)
     (set (map keyword trgs))]))

(defn parse [x]
  (into {} (map parse-line (str/split-lines x))))

(defn count-paths [g from to]
  (let [count-paths*
        (u/fn-memoized count-paths* [from]
          (if (= from to)
            1
            (let [next-nodes (g from)]
              (reduce +
                      (map (fn [x] (count-paths* x))
                           next-nodes)))))]
    (count-paths* from)))

(defn count-paths-2 [g from to]
  (let [count-paths*
        (u/fn-memoized count-paths* [from
                                     seen-fft
                                     seen-dac]
          (if (= from to)
            (if (and seen-dac seen-fft)
              1
              0)
            (let [next-nodes (g from)]
              (reduce +
                      (map (fn [x] (count-paths* x
                                                (or seen-fft (= x :fft))
                                                (or seen-dac (= x :dac))))
                           next-nodes)))))]
    (count-paths* from false false)))

(defn part-1 [in]
  (count-paths (parse in) :you :out))

(defn part-2 [in]
  (count-paths-2 (parse in) :svr :out))

(comment

  (user/graphviz-digraph
    (doseq [[src trgs] (parse ex)
            trg trgs]
      (println (name src) "->" (name trg))))

  (user/graphviz-digraph
    (doseq [[src trgs] (parse ex-2)
            trg trgs]
      (println (name src) "->" (name trg))))

  (user/graphviz-digraph
    #_    (println "overlap=scale")
    (doseq [[src trgs] (parse (user/day-input))
            trg trgs]
      (println (name src) "->" (name trg))))

  )


{:part-1 (part-1 (user/day-input))
 :part-2 (part-2 (user/day-input))}
