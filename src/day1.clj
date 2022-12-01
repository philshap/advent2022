(ns day1
  (:require
    [clojure.edn :as edn]
    [clojure.string :as str]))

(def input
  (-> "src/day1-input.txt"
      slurp
      (str/split #"\n\n")
      (->> (map (comp edn/read-string (partial format "[%s]"))))))

(def sorted-cals
  (->> input
       (map (partial reduce +))
       sort))

(defn part1 []
  (last sorted-cals))

(defn part2 []
  (->> sorted-cals
       reverse
       (take 3)
       (reduce +)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )