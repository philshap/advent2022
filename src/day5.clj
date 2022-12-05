(ns day5
  (:require [clojure.string :as str]))

;; stacks are a map of {stack-num => stack of creates}
;; a stack of crates is ordered from top to bottom
(defn parse-stacks [stacks-input]
  (let [data (str/split-lines stacks-input)
        stacks (->> (butlast data)
                    reverse
                    (map (comp #(map second %) #(partition 4 %) seq #(str % " "))))
        stack-nums (map parse-long (re-seq #"\d+" (last data)))]
    (reduce (fn [stack-map stack-num]
              (assoc stack-map
                stack-num
                (reduce (fn [stack row]
                          (let [crate (nth row (dec stack-num) \space)]
                            (if (not (= \space crate))
                              (conj stack crate)
                              stack)))
                        () stacks)))
            {} stack-nums)))

(defn parse-moves [moves]
  (->> (re-seq #"\d+" moves)
       (map parse-long)
       (partition 3)))

(def input
  (let [[stacks moves] (-> (slurp "src/day5-input.txt")
                           (str/split #"\n\n"))]
    [(parse-stacks stacks) (parse-moves moves)]))

(defn stack-tops [stacks]
  (->> (range 1 (inc (count stacks)))
       (map #(get stacks %))
       (map first)))

(defn move-crates [[stacks moves] do-move]
  (->> moves
       (reduce do-move stacks)
       stack-tops
       (apply str)))

(defn move1 [stacks [num from to]]
  (->> stacks
       (iterate
         (fn [stacks]
           (-> stacks
               (update to #(conj % (peek (stacks from))))
               (update from pop))))
       (drop num)
       first))

(defn part1 []
  (move-crates input move1))

(defn move2 [stacks [num from to]]
  (-> stacks
      (update to #(concat (take num (stacks from)) %))
      (update from #(drop num %))))

(defn part2 []
  (move-crates input move2))

; part 1:  MQSHJMWNH
; part 2:  LLWJRBHVZ

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )