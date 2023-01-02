(ns day22
  (:require [clojure.string :as str]))

(def input
  (slurp "src/day22-input.txt"))

(defn parse-board [data]
  (let [lines (str/split-lines data)]
    (->>
      (for [y (range (count lines))
            :let [line (nth lines y)]
            x (range (count line))
            :let [ch (nth line x)]
            :when (or (= ch \#) (= ch \.))]
        [[x y] ch])
      (into {}))))

(defn parse-moves [data]
  (re-seq #"\d+|[RL]" data))

(defn parse-input [input]
  ((juxt parse-board parse-moves) input))

;; Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^).
(def right 0)
(def down 1)
(def left 2)
(def up 3)

(def board-right
  (fn [board]
    (->> board keys (map first) (reduce max))))

(def board-bottom
  (fn [board]
    (->> board keys (map second) (reduce max))))

(def dir->delta {right [1 0], down [0 1], left [-1 0], up [0 -1]})

(defn find-edge [board dir [x y]]
  (->> (condp = dir
         right [0 y]
         down [x 0]
         left [(board-right board) y]
         up [x (board-bottom board)])
       (iterate #(mapv + (dir->delta dir) %))
       (drop-while #(nil? (board %)))
       first))

(defn move-one [board dir pos]
  (let [new-pos (mapv + (dir->delta dir) pos)]
    (case (board new-pos)
      \# pos
      \. new-pos
      (let [wrap-pos (find-edge board dir pos)]
        (case (board wrap-pos)
          \# pos
          \. wrap-pos)))))

(defn move-pos [board pos dir move]
  (let [num (parse-long move)]
    (if num
      (->> pos
           (iterate (partial move-one board dir))
           (drop (parse-long move))
           first)
      pos)))

(def move->rotate {"L" dec, "R" inc})

(defn move-turn [dir move]
  (let [rotate (move->rotate move)]
    (if rotate (mod (rotate dir) 4) dir)))

(defn do-move [board [pos dir] move]
  [(move-pos board pos dir move) (move-turn dir move)])

;; The final password is the sum of 1000 times the row, 4 times the column, and the facing.
(defn loc->password [[[x y] dir]]
  (+ (* (inc y) 1000)
     (* (inc x) 4)
     dir))

(defn initial-loc [board]
  [(find-edge board right [0 0]) right])

(defn part1 []
  (let [[board moves] (parse-input input)]
    (->> moves
         (reduce (fn [loc move]
                   (do-move board loc move)) (initial-loc board))
         loc->password)))

(defn part2 []
  )

;; part 1:  73346

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )