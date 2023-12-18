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
        path (format "%s/%s.txt" year day)
        f (io/resource path )]

    (if (.exists f)
      (let [s (slurp f)]
        (if (str/blank? s)
          (throw (ex-info "blank file" {:f path}))
          s)
        )
      (do
        (.createNewFile f)
        (throw (ex-info "no-such-file" {:f path}))))))
