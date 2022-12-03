(ns day3
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))

(def input
  (-> "src/day3-input.txt"
      slurp
      str/split-lines))

(defn score [item]
  (let [item (int item)
        a-int (int \a)]
    (if (and (>= item a-int) (<= item (int \z)))
      (inc (- item a-int))
      (+ 27 (- item (int \A))))))

(defn part1 []
  (->> input
       (map seq)
       (map #(partition (/ (count %) 2) %))
       (map (fn [[c1 c2]] (set/intersection (set c1) (set c2))))
       (map (comp score first))
       (reduce +)))

(defn part2 []
  (->> input
       (partition 3)
       (map (fn [x] (->> x (map (comp set seq)) (reduce set/intersection) first score)))
       (reduce +)))

; part 1:  8202
; part 2:  2864

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )