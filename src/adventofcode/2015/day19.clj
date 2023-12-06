(ns adventofcode.2015.day19
  (:require [clojure.string :as str]))

(defn parse-rule [rule]
  (vec   (rest (re-matches #"(.*) => (.*)" rule))))

(parse-rule "H => HO")


(defn parse [input]
  (let [ [rules _ start ]  (partition-by  #{""}(str/split-lines input))]


    {:rules (reduce
             (fn [acc [from to]]
               (update acc (keyword  from) (fnil conj []) (mapv keyword (re-seq #"[A-Z][a-z]?" to)))
               )
             {}
             (map parse-rule rules))
     :molecules (mapv keyword (re-seq #"[A-Z][a-z]?" (first start)))}))

(parse example)


(defn indices [vec f]
  )


(defn apply-rules [molecules rules]
  (->> (map-indexed (fn [i molecule]
                      (for [replacement (rules molecule)]
                        (vec (flatten (assoc molecules i replacement)))))
                    molecules)
       (apply concat)))

(defn shortest-steps [ start target rules]
  {:pre [(vector? start)]}
  (loop [todo [ [start 0]]
         seen #{}]

    (if-some [ state (first todo)]
      (let [ [molecules steps] tate ]
        (cond
          (= target molecules) steps
          (seen molecules) (recur (rest todo) seen)
          :else
          (let [next-molecules (apply-rules molecules rules)]
            (recur (concat (map #(vector % (inc steps)) next-molecules )
                           (rest todo))
                   (conj seen molecules)))))
      (throw (ex-info "not found" {:seen seen})))))

(defn reverse-rules [rules]
  (reduce-kv (fn [acc from tos]
               (reduce
                (fn [acc- to] (update acc- to (fnil conj []) from) )
                acc
                tos))
             {}
             rules))

(reverse-rules (:rules (parse example)))

(defn machtes [molreverse-rules])


(defn shortest-steps-bw [ start target rules]
  {:pre [(vector? start)]}
  (loop [todo [ [start 0]]
         seen #{}]

    (if-some [ state (first todo)]
      (let [ [molecules steps] state ]
        (cond
          (= target molecules) steps
          (seen molecules) (recur (rest todo) seen)
          :else
          (let [next-molecules (apply-rules molecules rules)]
            (recur (concat (map #(vector % (inc steps)) next-molecules )
                           (rest todo))
                   (conj seen molecules)))))
      (throw (ex-info "not found" {:seen seen})))))


(let [{:keys [molecules rules] } (parse input)]
  {:part-1 (count (distinct (apply-rules molecules rules)))
   :part-2 (shortest-steps [:e] molecules rules)
   })


(shortest-steps [:e] [:H :O :H] (:rules (parse "e => H
e => O
H => HO
H => OH
O => HH

HOH")))


(def example
  "H => HO
H => OH
O => HH

HOH")



(def input
  "Al => ThF
Al => ThRnFAr
B => BCa
B => TiB
B => TiRnFAr
Ca => CaCa
Ca => PB
Ca => PRnFAr
Ca => SiRnFYFAr
Ca => SiRnMgAr
Ca => SiTh
F => CaF
F => PMg
F => SiAl
H => CRnAlAr
H => CRnFYFYFAr
H => CRnFYMgAr
H => CRnMgYFAr
H => HCa
H => NRnFYFAr
H => NRnMgAr
H => NTh
H => OB
H => ORnFAr
Mg => BF
Mg => TiMg
N => CRnFAr
N => HSi
O => CRnFYFAr
O => CRnMgAr
O => HP
O => NRnFAr
O => OTi
P => CaP
P => PTi
P => SiRnFAr
Si => CaSi
Th => ThCa
Ti => BP
Ti => TiTi
e => HF
e => NAl
e => OMg

CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl")
