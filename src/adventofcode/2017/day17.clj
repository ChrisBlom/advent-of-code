(ns adventofcode.2017.day17)

(defn iter1 [step times]
  (loop [buffer [0]
         pos 0
         i 1]
    (if (<= i times)
      (let [new-pos (inc (mod (+ pos step) (count buffer)))
            [before after ]  (split-at new-pos buffer)
            new-buf (concat before [i] after)]
        #_(println new-buf new-pos)
        (recur new-buf
               new-pos
               (inc i)))
      (second (drop pos buffer )))))


(defn iter2 [step times]
  (loop [buffer [0]
         pos 0
         i 1]
    (when (zero? (rem i 1000000)) (println i))
    (if (<= i times)
      (let [new-pos (inc (mod (+' pos step) i))

            new-buf (if (= new-pos 1)
                      (let [[before after ] (split-at new-pos buffer)]
                        (concat before [i] after))
                      buffer)]
        (recur new-buf
               new-pos
               (inc i)))
      (second buffer))))


{:part-1 (iter1 380 2017)
 :part-2 (iter2 380 (* 50 1000 1000))}
