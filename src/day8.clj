(ns day8
  (:require [clojure.string :as str]))

(def input
  (let [lines (-> "src/day8-input.txt" slurp str/split-lines)]
    (->>
      (for [y (range (count lines))
            :let [line (nth lines y)]
            x (range (count line))]
        [[x y] (-> (nth line x) str parse-long)])
      (into {}))))

(def dirs [[0, -1], [-1, 0], [0, 1], [1, 0]])

(defn tree-at [trees pos]
  (trees pos))

(defn valid-pos? [trees pos]
  (some? (tree-at trees pos)))

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
  (->> (keys input)
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
  (->> (keys input)
       (map (partial scenic-score input))
       (reduce max)))

; part 1:  1789
; part 2:  314820

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )