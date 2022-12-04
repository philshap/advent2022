(ns day4
  (:require
    [clojure.string :as str]))

;; 2-4,6-8
(def input
  (-> "src/day4-input.txt"
      slurp
      (str/split #"[\-,\n]")
      (->>
        (map parse-long)
        (partition 4))))

(defn subset? [[low1 hi1 low2 hi2]]
  (or (<= low1 low2 hi2 hi1)
      (<= low2 low1 hi1 hi2)))

(defn part1 []
  (->> input
       (filter subset?)
       count))

(defn overlap? [[low1 hi1 low2 hi2]]
  (or (<= low1 low2 hi1) (<= low1 hi2 hi1)
      (<= low2 low1 hi2) (<= low2 hi1 hi2)))
(defn part2 []
  (->> input
       (filter overlap?)
       count))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )