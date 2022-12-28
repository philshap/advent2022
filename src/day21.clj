(ns day21
  (:require [clojure.string :as str]))

; nrwg: pqnb + wznq
; mcbv: 1

(def input
  (-> "src/day21-input.txt"
      slurp
      str/split-lines))

(defn parse-input [input]
  (reduce (fn [[values forms] line]
            (let [[_ name value] (re-find #"([a-z]{4}): (\d+)" line)
                  [_ out in1 op in2] (re-find #"([a-z]{4}): ([a-z]{4}) (.) ([a-z]{4})" line)]
              (if value
                [(assoc values name (parse-long value)) forms]
                [values (conj forms [out in1 op in2])])))
          [{} #{}]
          input))

(def op->op {"*" *, "+" +, "-" -, "/" /})

(defn reduce-one [[values forms]]
  (reduce (fn [_ [out in1 op in2 :as form]]
            (let [v1 (values in1)
                  v2 (values in2)]
              (if (and v1 v2)
                (reduced [(assoc values out ((op->op op) v1 v2)) (disj forms form)])
                [values forms])))
          nil
          forms))

(defn part1 []
  (->> (parse-input input)
       (iterate reduce-one)
       (drop-while (fn [[_ forms]] (not-empty forms)))
       ffirst
       (#(% "root"))))

(defn part2 []
  )

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )