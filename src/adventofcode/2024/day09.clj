(ns adventofcode.2024.day07
  (:require
   [adventofcode.utils :as u]
   [clojure.math :as math]
   [clojure.string :as str]))


(def ex "2333133121414131402")


(defn parse [ex]
  (map
   (fn [ id [file-size free-size]]
     {:id id
      :file-size file-size
      :free-size (or free-size 0)})
   (range)
   (partition-all 2
                  (map (comp parse-long str) ex))))

(parse ex)


(defn last-block [todo]
  (->> todo
       (map list (range))
       reverse
       (filter (comp pos? :file-size second))
       first))

(defn compact [todo]
  (println todo)
  (when-some [{:keys [id file-size free-size] :as block} (first todo)]
    (cond
      (zero? free-size)
      (cons block (compact (rest todo)))

      :else
      (let [todo (vec (rest todo))
            [i f] (last-block todo )
            {:keys [t id size]} f
            file-moved (min free-size size)
            file-remaining (- size file-moved)
            ]
        ;; todo handle free remainign
        [id file-moved (assoc todo
                              i (update f
                                        :file-size - file-moved
                                        :free-size + file-moved))]
        (cons
         ;; TODO merge same id
         {:id id
          :file-size file-size
          :free-size (- free-size file-moved)}
         todo)))))


(compact (parse "4321"))


(Defn parse [ex ]
  (:out (reduce (fn [{:keys [out free is-file id]} n]
                  {:out (conj out
                              (if is-file
                                {:t :file :id id :size n}
                                {:t :free :size n}))
                   :is-file (not is-file)
                   :id (if is-file (inc id) id)}
                  )

                {:out []
                 :is-file true
                 :id 0}
                (map (comp parse-long str) ex ))))


(group-by :t (parse ex))

10 15

(defn last-file [todo]
  (->> todo
       (map list (range))
       reverse
       (filter (comp #{:file} :t second))
       (filter (comp pos? :size second))
       first))

(last-file [{:t :free}
            {:t :free}
            {:t :file :id 'a :size 3}
            {:t :free}
            {:t :free}
            {:t :file :id 'b :size 0}
            {:t :free}])



(defn fill-free [todo free-size]
  (let [todo (vec todo)
        [i f] (last-file todo)
        {:keys [t id size]} f
        file-moved (min free-size size)
        file-remaining (- size file-moved)
        ]
    ;; todo handle free remainign
    [id file-moved (assoc todo
                           i (assoc f :size file-remaining)
                           ;; TODO append free space!
                           )]))

(fill-free [{:t :free}
            {:t :free}
            {:t :file :id 'b :size 3}
            {:t :free}]
           4
           )

(defn defrag [todo]
  (println (pp todo))
  (when-some [{:keys [t id size] :as x} (first todo)]
    (case t
      :file (cons x (defrag (rest todo)))
      :free
      (if (empty? (rest todo))
        todo
        (let [ [ id filled todo-] (fill-free (rest todo) size)]
          (cons {:t :file
                 :id id
                 :size filled}
                (defrag todo-)))))))


(defrag [{:t :file :id 'c :size 3}
         {:t :free :size 2}
         {:t :free :size 2}
         {:t :file :id 'b :size 3}
         {:t :file :id 'a :size 1}])


(defn pp [xx]
  (with-out-str
    (doseq [ {:keys [t id size]} xx]
      (case t
        :file
        (dotimes [_ size]
          (print id))
        :free
        (dotimes [_ size]
          (print \.))))))

(pp  (defrag (parse "931")))
(pp  (parse "931"))


(pp  (defrag (parse "22222")))
(pp  (parse "22222"))

0099811188827773336446555566..............
(pp (defrag (parse ex)))
