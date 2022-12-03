(ns day3
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))

(def input
  (-> "src/day3-input.txt"
      slurp
      str/split-lines))

(defn score [item]
  (let [item (int item)]
    (if (<= (int \a) item (int \z))
      (inc (- item (int \a)))
      (+ 27 (- item (int \A))))))

(defn group-and-score [size split-sack items]
  (->> items
       (map seq)
       (mapcat split-sack)
       (map set)
       (partition size)
       (map (comp score first #(reduce set/intersection %)))
       (reduce +)))

(defn split-in-half [items]
  (partition (/ (count items) 2) items))

(defn part1 []
  (group-and-score 2 split-in-half input))

(defn part2 []
  (group-and-score 3 list input))

; part 1:  8202
; part 2:  2864

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )