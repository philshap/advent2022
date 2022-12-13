(ns day12
  (:require [clojure.string :as str]))

(def input
  (let [lines (-> "src/day12-input.txt" slurp str/split-lines)
        grid (->>
               (for [y (range (count lines))
                     :let [line (nth lines y)]
                     x (range (count line))]
                 [[x y] (-> (nth line x) int)])
               (into {}))
        start (->> (keys grid) (filter #(= (int \S) (grid %))) first)
        end (->> (keys grid) (filter #(= (int \E) (grid %))) first)]
    ;; replace start and end vals to make can-move? check easier
    [(-> grid
         (assoc start (dec (int \a))
                end (inc (int \z))))
     start end]))

(def dirs [[-1 0] [1 0] [0 -1] [0 1]])

(defn can-move? [grid from to]
  (let [to-val (grid to)]
    (and to-val
         (>= (inc (grid from)) to-val))))

(def max-steps 9999)

(defn make-move [grid end [[pos & tail] visited]]
  (let [new-cost (inc (visited pos))
        moves (->> dirs
                   (map #(mapv + pos %))
                   (remove #(get visited %))
                   (filter #(can-move? grid pos %)))
        new-visited (merge visited (zipmap moves (repeat new-cost)))
        new-queue (concat tail moves)]
    (if (empty? new-queue)
      [() (assoc visited end max-steps)]
      [new-queue new-visited])))

(defn find-path [grid starts end]
  (->> [starts (merge (zipmap starts (repeat 0)))]
       (iterate (partial make-move grid end))
       (map (fn [[_ visited]] (visited end)))
       (filter some?)
       first))

(defn part1 []
  (let [[grid start end] input]
    (find-path grid [start] end)))

(defn find-starts [grid]
  (->> (keys grid) (filter #(= (grid %) (int \a)))))

(defn part2 []
  (let [[grid _ end] input]
    (find-path grid (find-starts grid) end)))

; part 1:  447
; part 2:  446

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )