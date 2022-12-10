(ns day10
  (:require [clojure.string :as str]))

(def input
  (-> "src/day10-input.txt"
      slurp
      str/split-lines))

(defn run-one [[[command & rest :as commands] x-reg carry]]
  (if (nil? carry)
    [rest x-reg (-> (str/split command #" ") last parse-long)]
    [commands (+ x-reg carry) nil]))

(defn signal [cycles regs]
  (map #(* % (nth regs (dec %))) cycles))

(defn part1 []
  (->> (iterate run-one [input 1 nil])
       (map second)
       (signal [20 60 100 140 180 220])
       (reduce +)))

(def width 40)
(def lines 6)

(defn to-pixel [pos [_ x-reg _]]
  (let [pos (mod pos width)]
    (if (<= (dec pos) x-reg (inc pos))
      \#
      \.)))

(defn part2 []
  (->> (iterate run-one [input 1 nil])
       (map-indexed to-pixel)
       (partition width)
       (take lines)
       (map (partial apply str))))

; part 1:  14620
; part 2:  BJFRHRFU
(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )