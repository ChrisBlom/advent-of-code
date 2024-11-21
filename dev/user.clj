(ns user
  (:require [portal.api :as p]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def portal-session (atom nil))

(defn portal []
  (if @portal-session
    (reset! portal-session (p/open {:window-title "AoC"}))
    (p/open @portal-session)))

(add-tap #'p/submit)

(defn day-input []
  (let [ [_ year day](str/split (str *ns*) #"\.")
        path (format "resources/%s/%s.txt" year day)
        f (io/file path )]

    (if (.exists f)
      (let [s (slurp f)]
        (if (str/blank? s)
          (throw (ex-info "blank file" {:f path}))
          s)
        )
      (do
        (.createNewFile f)
        (throw (ex-info "no-such-file" {:f path}))))))

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
