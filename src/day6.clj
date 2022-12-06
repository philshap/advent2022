(ns day6)

(def input (slurp "src/day6-input.txt"))

(defn find-unique [input size]
  (->> (seq input)
       (partition size 1)
       (map (comp count set))
       (take-while #(not= size %))
       count
       (+ size)))

(defn part1 []
  (find-unique input 4))

(defn part2 []
  (find-unique input 14))

; part 1:  1987
; part 2:  3059

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )