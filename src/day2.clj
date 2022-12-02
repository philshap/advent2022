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

(defn score-part1 [[them us]]
  (+ (inc us)
     (outcome->score
       (cond
        (= ((outcome->play draw) them) us) draw
        (= ((outcome->play lose) them) us) lose
        :else win))))

(defn part1 []
  (->> input
       (map score-part1)
       (reduce +)))

(defn score-part2 [[them outcome]]
  (+ (outcome->score outcome)
     (inc ((outcome->play outcome) them))))

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