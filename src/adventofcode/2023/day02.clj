(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[clojure.test :as t])

(defn parse-round [round]
  (->> (str/split round #", ")
       (map (fn [pair] (let [ [n color] (str/split pair #" ")]
                        [(keyword color)
                         (read-string n)])))
       (into {})))

(assert (= (parse-round "3 blue, 4 red") {:blue 3, :red 4}))

(defn parse-line [line]
  (let [ [_ id game] (re-matches #"Game (\d+): (.*)" line)]
    [(read-string id)
     (mapv parse-round (str/split game #"; " ))]))

(defn parse [input]
  (->> input
       str/split-lines
       (map parse-line)
       (into {})))

(def limits {:red 12, :green 13, :blue 14})

(defn possible? [limits round]
  (every? val (merge-with <= round limits)))

(defn part-1 [input]
  (->> input
       parse
       (filter (fn [[id game]] (every? (fn [rounds] (possible? limits rounds)) game)))
       (map key)
       (reduce + 0)))

(defn fewest-cubes-possible [rounds]
  (apply merge-with max rounds))

(defn part-2 [input]
  (->> input
       parse
       (map (fn [[id rounds]] (reduce * 1 (map val (fewest-cubes-possible rounds)))))
       (reduce + 0)))

(def example
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(assert (= 8 (part-1 example)))
(assert (= 2286 (part-2 example)))


(let [input (slurp (io/resource "2023/day02.txt"))]
  {:part-1 (part-1 input)
   :part-2 (part-2 input)})
