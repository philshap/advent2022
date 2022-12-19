(ns day17
  (:require
    [clojure.set :as set]
    [clojure.string :as str]))

(def input (-> (slurp "src/day17-input.txt")))

(defn make-rock [lines]
  (->>
    (for [y (range (count lines))
          :let [line (nth lines y)]
          x (range (count line))
          :let [ch (nth line x)]
          :when (= ch \#)]
      [x y])
    (into #{})))

;; may make things simpler to force bottom edge of rocks to be at row 0

;; The five types of rocks have the following peculiar shapes, where # is rock and . is empty space:
(def rocks
  (-> "####\n\n.#.\n###\n.#.\n\n..#\n..#\n###\n\n#\n#\n#\n#\n\n##\n##"
      (str/split #"\n\n")
      (->> (map (comp make-rock str/split-lines)))))

;; In jet patterns, < means a push to the left, while > means a push to the right.
(def jet->delta {\< [-1 0], \> [1 0]})

(def bottom 0)
;; The tall, vertical chamber is exactly seven units wide.
(def left 0)
(def right 7)

(defn make-chamber []
  (->> (for [x (range left right)]
         [x bottom])
       (into #{})))

(defn print-chamber [chamber]
  (doall
    (for [y (range (apply min (map second chamber)) (inc (apply max (map second chamber))))
          x (range left right)]
      (do
        (if (= x left) (print (format "%04d  " (abs y))))
        (if (chamber [x y])
          (print \#)
          (print \.))
        (if (= x (dec right))
          (println))))))

(defn highest-rock [chamber]
  (reduce min (map second chamber)))

(defn bottom-edge [rock]
  (reduce max (map second rock)))

(defn adjust-rock [rock delta]
  (->> (map #(mapv + delta %) rock)
       (into #{})))

;; Each rock appears so that its left edge is two units away from the left wall and its
;; bottom edge is three units above the highest rock in the room (or the floor, if there
;; isn't one).
(defn start-rock [chamber rock]
  (adjust-rock rock [(+ left 2) (- (highest-rock chamber) (+ 4 (bottom-edge rock)))]))

(defn place-rock [chamber rock]
  (set/union chamber rock))

(defn valid-rock? [chamber rock]
  (and (every? #(<= left (first %) (dec right)) rock)
       (empty? (set/intersection chamber rock))))

;; After a rock appears, it alternates between being pushed by a jet of hot gas one unit (in the
;; direction indicated by the next symbol in the jet pattern) and then falling one unit down. If
;; any movement would cause any part of the rock to move into the walls, floor, or a stopped rock,
;; the movement instead does not occur. If a downward movement would have caused a falling rock
;; to move into the floor or an already-fallen rock, the falling rock stops where it is (having
;; landed on something) and a new rock immediately begins falling.
(defn move-rock [[chamber rock [jet & jets]]]
  (let [maybe-push (adjust-rock rock (jet->delta jet))
        rock (if (valid-rock? chamber maybe-push) maybe-push rock)
        maybe-fall (adjust-rock rock [0 1])]
    ;(print-chamber (place-rock chamber rock))
    ;(println)
    (if (valid-rock? chamber maybe-fall)
      [chamber maybe-fall jets]
      [(place-rock chamber rock) nil jets])))

(defn drop-rock [chamber rock jets]
  (->> [chamber (start-rock chamber rock) jets]
       (iterate move-rock)
       (drop-while #(some? (second %)))
       first
       ((fn [[chamber _ jets]] [chamber jets]))))

(defn drop-next-rock [[chamber [rock & rocks] jets]]
  (let [[new-chamber new-jets] (drop-rock chamber rock jets)]
    [new-chamber rocks new-jets]))

(defn part1 []
  (->> [(make-chamber) (cycle rocks) (cycle input)]
       (iterate drop-next-rock)
       (drop 2022)
       ffirst
       highest-rock
       abs))

;; looking for patterns in the data by printing the highest rock every 1000 rocks.
;; In theory the number of layers added should be constant after (* (count rocks) (count input))
;; iterations. But that's 50455 iterations and will still take hours to run.
(defn part2 []
  (->> [(make-chamber) (cycle rocks) (cycle input)]
       (iterate drop-next-rock)
       (map first)
       (map (comp abs highest-rock))
       (map-indexed (fn [index top] (if (zero? (mod index 1000)) (println top) (flush))))
       last
       ;last
       ;(map second)
       ;(reduce min)
       ;abs
       ;first
       ))

;; part 1:  3114

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )