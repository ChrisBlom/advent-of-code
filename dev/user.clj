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
        f (io/file (str "resources/"  (format "%s/%s.txt" year day)))]

    (if (.exists f)
      (slurp f)
      (do
        (.createNewFile f)
        (throw (ex-info "no-such-file" {:f f}))
        )
      )))
