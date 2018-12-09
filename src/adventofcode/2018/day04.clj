(ns adventofcode.2018.day04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def ex
  (str/split-lines
   "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"))

(def input (line-seq (io/reader (io/resource "2018/day04.txt"))))

(defn ->long [s]
  (Long/parseLong s))

(defn parse-line [line]
  (let [[_ year month day hour minute event] (re-matches #".(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d). (.*)" line)]
    {:date [year month day]
     :time [(->long hour) (->long minute)]
     :event event}))

(defn get-guard-id [s]
  (->long (second (re-matches #"Guard #(\d+) begins shift" s))))

(def vconj (fnil conj []))

(defn process [{:keys [current-guard asleep-since] :as accum} {:keys [date time event] :as e}]
  (let [ [hour minute] time
        sdate (str/join "-" date)]
    (condp #(str/starts-with? %2 %1) event
      "Guard" (let [guard (get-guard-id event)
                    ev2 [:start-of-shift guard sdate time]]
                (-> accum
                    (assoc :current-guard guard)
                    (update-in [:by-guard guard] vconj ev2)))
      "falls"
      (let [ev2 [:falls-asleep current-guard sdate time]]
        (-> accum
            (update-in [:by-guard current-guard] conj ev2)))
      "wakes"
      (let [ev2 [:awakes current-guard sdate time]]
        (-> accum
            (update-in [:by-guard current-guard] conj ev2))))))

(->> ex
     (map parse-line)
     (reduce process {}))

(defn update-vals [f ms]
  (reduce-kv (fn [m k v]
               (assoc m k (f v)))
             ms
             ms))

(defn process-guard-event [{:keys [fell-asleep-at date->minute->slept] :as accum} [event guard date [hour minute]] ]
  (case event
    :start-of-shift (-> accum (dissoc :fell-asleep-at) )
    :falls-asleep (-> accum (assoc :fell-asleep-at minute))
    :awakes (-> accum
                (dissoc :fell-asleep-at)
                (update-in [:guard->slept guard] (fnil + 0) (- minute fell-asleep-at) )
                (update-in [:guard->minute->slept guard]
                           (fn [minute->slept]
                             (reduce
                              (fn [arr minute]
                                (update-in arr [minute guard] (fnil inc 0) ))
                              (or minute->slept (into (sorted-map) (take 60 (map list (range 0) (repeat {})))))
                              (range fell-asleep-at minute)))))))

(let [{:keys [guard->slept guard->minute->slept] :as a}
      (-> input
          (->> (map parse-line)
               (sort-by second)
               (sort-by first)
               (reduce process {})
               :by-guard
               (update-vals #(reduce process-guard-event {} %)))
          vals
          (->> (apply merge-with merge)))
      [most-sleeping-guard slept] (apply max-key val guard->slept)
      [most-slept-minute] (->> (get guard->minute->slept most-sleeping-guard)
                               (apply max-key #(get (val %) most-sleeping-guard)))
      guard-id most-sleeping-guard]
  {:part-1 (* most-slept-minute guard-id)
   :part-2
   (let [ [minute [guard-id amount]]
         (->>
          guard->minute->slept
          vals
          (apply (partial merge-with (partial merge-with +)))
          (update-vals #(apply max-key val %))
          (apply max-key (comp second val))
          )]
     (* minute guard-id))})
