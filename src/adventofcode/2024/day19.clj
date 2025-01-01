(ns adventofcode.2024.day19
  (:require
   [adventofcode.utils :as u]
   [clojure.math :as math]
   [clojure.string :as str]))

(def ex "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(defn parse [x]
  (let [ [towels patterns] (str/split x #"\n\n")]
    {:towels (str/split towels #", ")
     :patterns (str/split-lines patterns)}))


(defn re [ss]
  (format "(%s)+"
          (str/join "|" (map (partial format "(%s)") ss))))

(defn part-1 [input]
  (let [{:keys [towels patterns]} (parse input)
        rx (re-pattern (re towels))]
    (count (keep (fn [pattern]
                   (re-matches rx pattern))
                 patterns))))

(part-1 ex)

(part-1 (user/day-input))

(defn trie-add [trie x]
  (update-in trie x merge {:end true}))

(-> {}
    (trie-add "aaa")
    (trie-add "aab"))

(defn count-matches
  ([trie s]
   (count-matches (atom {}) trie trie s))
  ([ch root-trie cur-trie [h & t :as x]]
   (if-not cur-trie
     0
     (if-let [c (get-in @ch [x cur-trie])]
       c
       (let [cn (let [final (:end cur-trie)]
                  (+ (if (and (empty? t)
                              final) 1 0)
                     (count-matches ch root-trie
                                    (cur-trie h)
                                    t)
                     ;; start new match after end
                     (if (and final (seq t))
                       (count-matches ch root-trie
                                      (root-trie h)
                                      t)
                       0)))]
         (swap! ch assoc-in [x cur-trie] cn)
         cn
         )))))

(count-matches
 (reduce trie-add {} ["g" "b" "r" "br" "gb"])
 "gbbr"
 )

(count-matches
 (reduce trie-add {} ["g" "b" "r" "br" "gb" "rb"])
 "rrbgbr"
 )

(defn part-2 [input]
  (let [{:keys [towels patterns]} (parse input)
        trie (reduce trie-add {} towels)]
    (mapv (fn [pattern]
            (count-matches
             trie
             pattern))
          patterns)))

(assert (= 16 (part-2 ex)))

{:part-1 (part-1 (user/day-input))
 :part-2 (time (part-2 (user/day-input)))}


(comment



  )
