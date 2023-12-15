(ns adventofcode.2023.day15
  (:require [clojure.string :as str]))

(defn hash-code [s]
  (reduce
   (fn [v c]
     (-> (int c)
         (+ v)
         (* 17)
         (rem 256)))
   0
   s))

(assert (= (hash-code "HASH")
           52))

(def ex "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn parse [s]
  (first (str/split-lines s)))

(defn part-1 [s]
  (reduce + (map hash-code (str/split s #","))))

(defn parse-label [x]
  (let [ [_ label op num] (re-matches #"([a-z]+)(\-|=)(\d*)" x)]
    (cond-> [label op]
      (= op "=") (conj (read-string num)))))

(parse-label "rn=1")

(defn lens-with-label-index [lenses label]
  (->> (map list lenses (range))
       (filter (comp #{label} ffirst))
       first
       last
       ))

(lens-with-label-index [["ab" 2] ["rn" 1]] "rn" )
(lens-with-label-index [ ["rn" 1]] "asd" )

(defn add-it [boxes label num]
  (let [current-box (hash-code label)]
    (if-let [idx (lens-with-label-index (boxes current-box) label)]
      (assoc-in boxes [current-box idx] [label num])
      (update boxes  current-box (fnil conj []) [label num]))))

(defn remove-it [boxes label]
  (let [current-box (hash-code label)]
    (if-let [idx (lens-with-label-index (boxes current-box) label)]
      (let [removed-label (second (get-in boxes [current-box idx]))]
        (update boxes current-box (fn [ls] (vec (remove (fn [[ll ln]] (= ll label)) ls)))))
      boxes)))

(remove-it {(hash-code "ab") [["ab" 123]]} "ab")
(remove-it {(hash-code "ab") [["as" 123]]} "ab")

(add-it {(hash-code "ab") [["ab" 123]]} "ab" 456)
(add-it {} "ab" 123)

(defn install-lenses [inst]
  (let [ instr (map parse-label (str/split inst #","))]
    (reduce (fn [ boxes [label op num]]
              (case op
                "=" (add-it boxes label num)
                "-" (remove-it boxes label)))
            (sorted-map)
            instr)))

(defn box-focussing-power [box-idx lenses]
  (map-indexed
   (fn [lens-idx [label focal-len]]
     (*
      (inc box-idx)
      (inc lens-idx)
      focal-len))
   lenses))

(defn focusing-power [instr]
  (->> instr
       (mapcat (fn [ [box-idx lenses] ] (box-focussing-power box-idx lenses)))
       (reduce +)))

{:part-1 (part-1 (parse (user/day-input)))
 :part-2 (focusing-power (install-lenses (parse (user/day-input))))}
