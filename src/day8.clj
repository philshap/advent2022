(ns day8
  (:require [clojure.string :as str]))

(def input
  (-> "src/day8-input.txt"
      slurp
      str/split-lines
      (->>
        (map (partial re-seq #"\d"))
        (mapv (comp vec #(map (comp parse-long) %))))))

(def dirs [[0, -1], [-1, 0], [0, 1], [1, 0]])

(defn tree-at [trees [x y]]
  (nth (nth trees x) y))

(defn valid-pos? [trees [x y]]
  (and (<= 0 x (dec (count trees)))
       (<= 0 y (dec (count trees)))))

(defn visible-in-dir [trees pos height dir]
  (let [next-pos (mapv + pos dir)]
    (or (not (valid-pos? trees next-pos))
        (and (< (tree-at trees next-pos) height)
             (recur trees next-pos height dir)))))

(defn visible? [trees pos]
  (->> dirs
       (map (partial visible-in-dir trees pos (tree-at trees pos)))
       (some true?)))

(defn part1 []
  (->> (for [x (range (count input))
             y (range (count (first input)))]
         [x y])
       (filter (partial visible? input))
       count))

(defn score-in-dir [trees pos height dir]
  (let [next-pos (mapv + pos dir)]
    (if (not (valid-pos? trees next-pos))
      0
      (inc (if (< (tree-at trees next-pos) height)
             (score-in-dir trees next-pos height dir)
             0)))))

(defn scenic-score [trees pos]
  (->> dirs
       (map (partial score-in-dir trees pos (tree-at trees pos)))
       (reduce *)))

(defn part2 []
  (->> (for [x (range (count input))
             y (range (count (first input)))]
         [x y])
       (map (partial scenic-score input))
       (reduce max)))

; part 1:  1789
; part 2:  314820

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )