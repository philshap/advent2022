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

(defn visible-in-dir [trees pos height dir]
  (let [[x y :as next-pos] (mapv + pos dir)]
    (or (< x 0) (>= y (count (first trees)))
        (< y 0) (>= x (count trees))
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

(defn score [trees pos height dir]
  (let [[x y :as next-pos] (mapv + pos dir)]
    (if (or (< x 0) (>= y (count (first trees)))
            (< y 0) (>= x (count trees)))
      0
      (+ 1 (if (< (tree-at trees next-pos) height)
             (score trees next-pos height dir)
             0)))))

(defn scenic-score [trees pos]
  (->> dirs
       (map (partial score trees pos (tree-at trees pos)))
       (reduce *)))

(defn part2 []
  (->> (for [x (range (count input))
             y (range (count (first input)))]
         [x y])
       (map (partial scenic-score input))
       (reduce max)))

; part 1:  1789
; part 2:  314820

;(comment
(println "part 1: " (part1))
(println "part 2: " (part2))
;)