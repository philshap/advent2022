(ns day6)

(def input (slurp "src/day6-input.txt"))

(defn find-unique [input size]
  (->> (range (count input))
       (drop-while #(not= size
                          (->> (+ % size)
                               (subs input %)
                               set
                               count)))
       first
       (+ size)))

(defn part1 []
  (find-unique input 4))

(defn part2 []
  (find-unique input 14))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )