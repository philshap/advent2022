(ns day14
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (->> (re-seq #"\d+" line)
       (map parse-long)
       (partition 2)))

(def input
  (-> "src/day14-input.txt"
      slurp
      str/split-lines
      (->> (map parse-line))))

(defn draw-line [[[x1 y1] [x2 y2]]]
  (for [x (range (min x1 x2) (inc (max x1 x2)))
        y (range (min y1 y2) (inc (max y1 y2)))]
    {[x y] \#}))

(defn draw-lines [line]
  (->> (partition 2 1 line)
       (mapcat draw-line)
       (reduce merge)))

(defn make-grid [input]
  [(->> input (map draw-lines) (reduce merge))
   (->> input (apply concat) (map second) (reduce max))])

(def dirs [[0 1] [-1 1] [1 1]])

(defn move-sand-down1 [bottom [grid [_ y :as pos]]]
  (if (> y bottom)
    [grid :bottom]
    (->> dirs
         (map #(mapv + % pos))
         (remove #(grid %))
         first
         (#(if % [grid %] [(assoc grid pos \o) :placed])))))

(defn drop-sand [move-sand-down bottom grid]
  (->> [grid [500 0]]
       (iterate (partial move-sand-down bottom))
       (drop-while (fn [[_ pos]] (vector? pos)))
       ffirst))

(defn pour-sand [move-sand-down input]
  (let [[grid bottom] (make-grid input)]
    (->> grid
         (iterate (partial drop-sand move-sand-down bottom))
         (map vals)
         (map (fn [x] (filter #(= % \o) x)))
         (map count)
         (partition 2 1)
         (drop-while #(not= (first %) (second %)))
         ffirst)))

(defn part1 []
  (pour-sand move-sand-down1 input))

;; very slow, takes almost 2 min to run. could optimize case when
;; sand hits bottom and fill in sand diagonally to left and right
;; without dropping from the top.
(defn move-sand-down2 [bottom [grid pos]]
  (->> dirs
       (map #(mapv + % pos))
       (remove (fn [[_ y :as new-pos]] (or (grid new-pos) (= y (+ bottom 2)))))
       first
       (#(if % [grid %] [(assoc grid pos \o) :placed]))))

(defn part2 []
  (pour-sand move-sand-down2 input))

; part 1:  885
; part 2:  28691

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )