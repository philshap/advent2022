(ns day2
  (:require [clojure.string :as str]))

(defn char->val [ch base]
  (- (int ch) (int base)))

;; A/X - rock = 0
;; B/Y - paper = 1
;; C/Z - scissors = 2
(defn parse-line [[them _ us]]
  [(char->val them \A) (char->val us \X)])

(def input
  (-> "src/day2-input.txt"
      slurp
      str/split-lines
      (->> (map parse-line))))

(def lose 0)
(def draw 1)
(def win 2)
(defn outcome->score [outcome]
  (* 3 outcome))

(def outcome->play
  {lose #(mod (dec %) 3),
   draw identity,
   win  #(mod (inc %) 3)})

(defn strategy-score [input make-move]
  (->> input
       (map make-move)
       (map (fn [[move outcome]] (+ (inc move) (outcome->score outcome))))
       (reduce +)))

(defn part1-move [[them us]]
  [us (cond
        (= ((outcome->play draw) them) us) draw
        (= ((outcome->play lose) them) us) lose
        :else win)])

(defn part1 []
  (strategy-score input part1-move))

(defn part2-move [[them outcome]]
  [((outcome->play outcome) them) outcome])

(defn part2 []
  (strategy-score input part2-move))

; part 1:  14375
; part 2:  10274

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )