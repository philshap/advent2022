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
    {(line->int 0)
     [(str->ints (second lines))
      0
      #((if (= op "+") + *) % (or (parse-long arg) %))
      #(if (zero? (rem % (line->int 3)))
         (line->int 4)
         (line->int 5))
      ;; collect all divisors
      (line->int 3)]}))

(defn make-monkeys [input]
  (->> (map make-monkey input)
       (reduce merge)))

(defn throw-to-monkey [monkeys monkey-num item]
  (update monkeys monkey-num (fn [[items count op1 op2]]
                               [(conj items item) count op1 op2])))

(defn do-round [handle-worry monkeys]
  (reduce
    (fn [monkeys monkey-num]
      (let [[items inspects update-worry throw-next] (monkeys monkey-num)]
        (reduce
          (fn [monkeys item]
            (let [new-worry (handle-worry (update-worry item))]
              (throw-to-monkey monkeys (throw-next new-worry) new-worry)))
          (assoc monkeys monkey-num [[] (+ inspects (count items)) update-worry throw-next])
          items)))
    monkeys (sort (keys monkeys))))

(defn monkey-business [monkeys rounds handle-worry]
  (->> monkeys
       (iterate (partial do-round handle-worry))
       (#(nth % rounds))
       vals
       (map second)
       sort
       (take-last 2)
       (reduce *)))

(defn part1 []
  (monkey-business (make-monkeys input) 20 #(quot % 3)))

(defn part2 []
  (let [monkeys (make-monkeys input)
        ;; all divisors happen to be primes, so lcm is (reduce *)
        lcm (->> (vals monkeys) (map last) (reduce *))]
    (monkey-business monkeys 10000 #(mod % lcm))))

;part 1:  98280
;part 2:  17673687232

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )