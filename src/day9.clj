(ns day9
  (:require [clojure.string :as str]))

(def input
  (-> "src/day9-input.txt"
      slurp
      str/split-lines
      ))

(def move->delta {\U [0 -1], \D [0 1], \L [-1 0], \R [1 0]})

(defn move-head [head move]
  (mapv + (move->delta move) head))

(def two->one {2 1, -2 -1, 1 1, -1 -1})

(defn move-tail [head [tx ty :as tail]]
  (let [[dx dy] (mapv - head tail)
        ddx (two->one dx)
        ddy (two->one dy)]
    ;(println head tail dx dy ddx ddy)
    (cond
      (and (not= 2 (abs dx)) (not= 2 (abs dy))) tail
      (zero? dx) [tx (+ ddy ty)]
      (zero? dy) [(+ ddx tx) ty]
      :else [(+ ddx tx) (+ ddy ty)])))

(defn move-one [move [head tail visited]]
  (let [new-head (move-head head move)
        new-tail (move-tail new-head tail)]
    ;(println move new-head new-tail)
    [new-head new-tail (conj visited new-tail)]))

(defn move-rope [state [move _ & num]]
  ;(println state move num)
  (->> state
       (iterate (partial move-one move))
       (drop (parse-long (apply str num)))
       first))

(defn part1 []
  (-> (reduce move-rope [[0 0] [0 0] #{}] input)
      last
      count))

(defn part2 []
  )

;(comment
(println "part 1: " (part1))
(println "part 2: " (part2))
;)