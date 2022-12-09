(ns day9
  (:require [clojure.string :as str]))

(def input
  (-> "src/day9-input.txt"
      slurp
      str/split-lines))

(def move->delta {\U [0 -1], \D [0 1], \L [-1 0], \R [1 0]})

(defn move-head [head move]
  (mapv + (move->delta move) head))

(def delta->move {2 1, -2 -1, 1 1, -1 -1, 0 0})

(defn move-tail [head [tx ty :as tail]]
  (let [[dx dy] (mapv - head tail)]
    (if (or (= 2 (abs dx)) (= 2 (abs dy)))
      [(+ (delta->move dx) tx) (+ (delta->move dy) ty)]
      tail)))

(defn move-one [move [[head & tails] visited]]
  (let [new-head (move-head head move)
        new-rope (reduce
                   (fn [new-rope tail]
                     (conj new-rope (move-tail (last new-rope) tail)))
                   [new-head] tails)]
    [new-rope (conj visited (last new-rope))]))

(defn move-rope [state [move _ & num]]
  (->
    (iterate (partial move-one move) state)
    (nth (parse-long (apply str num)))))

(defn make-rope [size]
  (repeatedly size (constantly [0 0])))

(defn play-rope-game [moves rope-length]
  (-> (reduce move-rope [(make-rope rope-length) #{}] moves)
      last
      count))

(defn part1 []
  (play-rope-game input 2))

(defn part2 []
  (play-rope-game input 10))

; part 1:  5710
; part 2:  2259

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )