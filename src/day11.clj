(ns day11
  (:require [clojure.string :as str]))

(def input
  (-> (slurp "src/day11-input.txt")
      (str/split #"\n\n")))

(defn str->ints [s] (map parse-long (re-seq #"\d+" s)))
(defn str->int [s] (first (str->ints s)))

;0 Monkey 0:
;1  Starting items: 56, 52, 58, 96, 70, 75, 72
;2  Operation: new = old * 17
;3  Test: divisible by 11
;4    If true: throw to monkey 2
;5    If false: throw to monkey 3

(defn make-monkey [data]
  (let [lines (str/split-lines data)
        [op arg] (take-last 2 (str/split (nth lines 2) #" "))
        line->int #(str->int (nth lines %))]
    [(str->ints (second lines))
     0
     #((if (= op "+") + *) % (or (parse-long arg) %))
     #(if (zero? (rem % (line->int 3)))
        (line->int 4)
        (line->int 5))]))

(defn make-monkeys [input]
  (->> input
       (map make-monkey)
       (map-indexed hash-map)
       (apply merge)))

(defn throw-to-monkey [monkeys monkey-num item]
  (update monkeys monkey-num (fn [[items count op1 op2]] [(conj items item) count op1 op2])))

(defn do-round [monkeys]
  (reduce (fn [monkeys monkey-num]
            (let [[items inspects update-worry throw-next] (monkeys monkey-num)]
              (reduce (fn [monkeys item]
                        (let [new-worry (quot (update-worry item) 3)]
                          (throw-to-monkey monkeys (throw-next new-worry) new-worry)))
                      (assoc monkeys monkey-num [[] (+ inspects (count items)) update-worry throw-next])
                      items)))
          monkeys (sort (keys monkeys))))

(defn part1 []
  (->> (make-monkeys input)
       (iterate do-round)
       (#(nth % 20))
       (#(update-vals % second))
       vals
       sort
       (take-last 2)
       (reduce *)))

;; part2 is wrong..
(defn part2 []
  (->> (make-monkeys input)
       (iterate do-round)
       (#(nth % 10000))
       (#(update-vals % second))
       vals
       sort
       (take-last 2)
       (reduce *)))

;part 1:  98280
;part 2:  28216440504

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )