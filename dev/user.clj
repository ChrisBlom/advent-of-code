(ns user
  (:require [portal.api :as p]
            [clojure.string :as str]
            [clojure.java.shell :as sh]
            [clojure.java.io :as io]))

(def portal-session (atom nil))

(defn portal []
  (if @portal-session
    (reset! portal-session (p/open {:window-title "AoC"}))
    (p/open @portal-session)))

(add-tap #'p/submit)

(def year "2024")
(def day "1")

(defn load-session-token []
  (slurp "session-token"))

(defn resource-path [year day]
  (format "resources/%s/day%s.txt" year (format "%02d" day)))

(defn download [year day]
  (let [{:keys [out err exit] :as resp} (sh/sh
                                         "curl" (str "https://adventofcode.com/" (parse-long (str year)) "/day/" (parse-long (str day)) "/input")
                                "-H" (str "Cookie: session=" (load-session-token)))]
    (if (zero? exit)
      (do
        (clojure.java.io/make-parents (resource-path year day))
        (spit (resource-path year day) out)
        out)
      (throw (ex-info "failed to download" {:year year, :day day, :resp resp})))))


(defn read-or-download [^long year ^long day]
  (let [path (resource-path year day)
        f (io/file path )]
    (if (.exists f)
      (let [s (slurp f)]
        (if (str/blank? s)
          (throw (ex-info "blank file" {:f path}))
          s)
        )
      (download year day))))


(defn day-input []
  (let [ [_ year day]
        (re-matches #"adventofcode\.(\d+).day(\d+)" (str *ns*))]
    (read-or-download (parse-long year) (parse-long day))))

(comment

  (read-or-download 2019 1)


  (doseq [y (range 2015 2024)
          d (range 1 26)]
    (Thread/sleep 100)
    (read-or-download y d))

  )

(defn show-dot
  ([s] (show-dot s :neato))
  ([s cmd ]
   (spit "out.dot" s)
   (let [{:keys [err out exit]}  (clojure.java.shell/sh (name cmd) "-Tsvg" "-O" "out.dot")]
     (assert (zero? exit) err))
   (clojure.java.browse/browse-url (str "file://" (.getAbsolutePath (clojure.java.io/file "out.dot.svg"))))))



(defmacro graphviz [& args]
  (let [ [cmd body] (if (keyword (first args))
                      [ (first args) (rest args)]
                      [ :neato args])]
    `(show-dot
      (with-out-str
        ~@body))))

(defmacro graphviz-digraph [& args]
  (let [ [cmd body] (if (keyword (first args))
                      [ (first args) (rest args)]
                      [ :neato args])]
    `(show-dot
      (with-out-str
        (println "digraph {")
        ~@body)
        (println "}")
      )))


(defn distinct-by [f coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[x :as xs] seen]
                   (when-let [s (seq xs)]
                     (let [k (f x)]
                       (if (contains? seen x)
                         (recur (rest s) seen)
                         (cons x (step (rest s) (conj seen x)))))))
                 xs seen)))]
    (step coll #{})))


(defn setup-day [year day]
  (let [path (io/file (format "src/adventofcode/%d/day%02d.clj" year day))]
    (if (.exists path)
      :already-exists
      (do
        (io/make-parents path)
        (-> (slurp (io/resource "template.clj"))
            (str/replace "<year>" (str year))
            (str/replace "<day>"  (format "%02d" day))
            (->> (spit path)))
        (read-or-download year day)))))


(import '[java.time LocalDate])

(defn setup-today []
  (let [today (LocalDate/now)]
    (setup-day
     (.getYear today)
     (.getDayOfMonth today))))
