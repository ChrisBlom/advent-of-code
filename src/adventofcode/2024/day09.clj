(ns adventofcode.2024.day09
  (:require
   [adventofcode.utils :as u]
   [clojure.math :as math]
   [clojure.string :as str]))


(def ex "2333133121414131402
")

(defn parse [ex]
  (mapv
   (fn [ id [file-size free-size]]
     {:id id
      :file-size file-size
      :free-size (or free-size 0)})
   (range)
   (partition-all 2
                  (map (comp parse-long str)
                       (first (str/split-lines ex))))))

(defn render [blocks]
  (with-out-str
    (doseq [ {:keys [id file-size free-size]} blocks]
      (dotimes [_ file-size]
        (print id))
      (dotimes [_ free-size]
        (print \.))0)))

(defn id-seq [blocks]
  (mapcat
   (fn [{:keys [id file-size free-size]}]
     (concat
      (repeat file-size id)
      (repeat free-size nil)))
   blocks))

(defn checksum [blocks]
  (apply +
         (map-indexed (fn [i c]
                        (* i (or c 0)))
                      (id-seq blocks))))

(defn find-last-block [todo]
  (->> todo
       (keep-indexed (fn [i b]
                       (when (pos? (:file-size b))
                         [i b])))
       last))

(defn compact
  ([blocks] (compact [] blocks))
  ([done todo]
   (when-some [{:keys [id file-size free-size] :as block} (first todo)]
     (cond
       (zero? free-size)
       (recur (conj done block)
              (rest todo))
       :else
       (let [tail (rest todo)
             [i move-block] (find-last-block tail )]
         (if-not move-block
           (concat done todo)
           (let [file-moved (min free-size (:file-size move-block))]
             (recur
              (conj done (assoc block :free-size 0))
              (cons
               ;; TODO merge same id
               {:id (:id move-block)
                :file-size file-moved
                :free-size (- free-size file-moved)}
               (assoc (vec tail)
                      i (-> move-block
                            (update :file-size - file-moved)
                            (update :free-size + file-moved))))))))))))


(assert (= (render (parse "12345"))
           "0..111....22222"))

(assert (= (render (compact (parse "12345")))
           "022111222......"))


(defn find-free-block [blocks search-size]
  (let [n (count blocks)]
    (loop [i 0]
      (if (>= i n)
        nil
        (let [b (blocks i)]
          (if (>= (:free-size (blocks i)) search-size)
            [i b]
            (recur (inc i))))))))

(defn insert-after [v i a]
  (->
   (subvec v 0 (inc i))
   (conj a)
   (into (subvec v (inc i) (count v)))))


(insert-after [1 2 3] 0 'a)
(insert-after [1 2 3] 1 'a)
(insert-after [1 2 3] 2 'a)

(defn defrag
  ([blocks] (defrag (list) blocks))
  ([done todo]
   (if-some [{:keys [id file-size free-size] :as block-to-move} (peek todo)]
     ^{:break/when (= id 4)}
     (cond

       (or (:moved block-to-move)
           (zero? file-size))
       (recur (conj done block-to-move)
              (pop todo))

       :else
       (let [tail (pop todo)
             [i move-to-block] (find-free-block tail file-size )]
         (if-not move-to-block

           (recur
            (conj done block-to-move)
            tail)

           (recur
            (conj done (-> block-to-move
                           (assoc :file-size 0)
                           (assoc :free-size (+ file-size
                                                free-size))))

            (-> tail
                (assoc i (-> move-to-block
                             (assoc :free-size 0)))
                (insert-after i
                              (-> block-to-move
                                  (assoc
                                   :moved true
                                   :free-size (-
                                               (:free-size move-to-block)
                                               (:file-size block-to-move))))))))))
     done
     )))

(assert
 (= (render (defrag (parse ex)))
    "00992111777.44.333....5555.6666.....8888.."))


;; TODO don't render to string, input has multi digit ids


(checksum (parse "1234"))

(assert (= (render (compact (parse ex)))
           "0099811188827773336446555566.............."))

(assert (= (checksum (compact (parse ex)))
           1928))


(defn part-1 [in]
  (checksum (compact (parse in))))

(defn part-2 [in]
  (checksum (defrag (parse in))))


(assert (= (part-2 ex) 2858))


{:part-1 (time (part-1 (user/day-input)))
 :part-2 (time (part-2 (user/day-input)))}
