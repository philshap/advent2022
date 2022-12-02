(ns day2
  (:require [clojure.string :as str]))

(defn char->val [ch base]
  (inc (- (int ch) (int base))))

;; A/X - rock = 1
;; B/Y - paper = 2
;; C/Z - scissors = 3
(defn parse-line [[them _ us]]
  [(char->val them \A) (char->val us \X)])

(def input
  (-> "src/day2-input.txt"
      slurp
      str/split-lines
      (->> (map parse-line))))

(def lose 1)
(def draw 2)
(def win 3)
(def outcome->score {win 6, draw 3, lose 0})

(def outcome->play
  {lose #(inc (mod (inc %) 3)),
   draw identity,
   win #(inc (mod % 3))})

(defn score-part1 [[them us]]
  (+ us
     (outcome->score
       (cond
        (= ((outcome->play draw) them) us) draw
        (= ((outcome->play lose) them) us) lose
        :else win))))

(defn part1 []
  (->> input
       (map score-part1)
       (reduce +)))

;; 1 - lose, 2 - draw, 3 - win
(defn score-part2 [[them outcome]]
  (+ (outcome->score outcome)
     ((outcome->play outcome) them)))

(defn part2 []
  (->> input
       (map score-part2)
       (reduce +)))

; part 1:  14375
; part 2:  10274

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )