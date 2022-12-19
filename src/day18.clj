(ns day18)

(def input
  (->> (slurp "src/day18-input.txt")
       (re-seq #"\d+")
       (map parse-long)
       (partition 3)))

;; FIXME create using `for`
;; A 1x1x1 cube has six sides.
(def sides
  [[[0 0 0] [0 0 1] [0 1 1] [0 1 0]]
   [[0 0 0] [0 0 1] [1 0 1] [1 0 0]]
   [[0 0 0] [1 0 0] [1 1 0] [0 1 0]]
   [[1 1 1] [0 1 1] [1 1 0] [0 1 0]]
   [[1 1 1] [0 1 1] [0 0 1] [1 0 1]]
   [[1 1 1] [1 1 0] [1 0 0] [1 0 1]]])

;; A cube is a list of six sides. Each side is a set of four points.
(defn make-cube [xyz]
  (map (fn [side] (into #{} (map #(mapv + xyz %) side))) sides))

(defn part1 []
  (let [cubes (map make-cube input)]
    ;; Use frequencies to find any side that's shared with another cube. The unique sides are
    ;; the total possible sides minus the number of shared sides (frequencies > 1).
    (- (* (count sides) (count cubes))
       (->> cubes
            flatten
            frequencies
            (map second)
            (filter #(> % 1))
            (reduce +)))))

(defn part2 []
  )

; part 1:  4604
; part 2:  nil

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )