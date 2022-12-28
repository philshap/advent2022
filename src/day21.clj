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
                [values (assoc forms out [in1 op in2])])))
          [{} {}]
          input))

(def op->op {"*" *, "+" +, "-" -, "/" /})

(defn reduce-one [[values forms]]
  (reduce-kv (fn [_ out [in1 op in2]]
            (let [v1 (values in1)
                  v2 (values in2)]
              (if (and v1 v2)
                (reduced [(assoc values out ((op->op op) v1 v2)) (dissoc forms out)])
                [values forms])))
          nil
          forms))

(defn part1 []
  (->> (parse-input input)
       (iterate reduce-one)
       (drop-while (fn [[_ forms]] (not-empty forms)))
       ffirst
       (#(% "root"))))

(def op->arg1-missing {"*" /, "+" -, "-" +, "/" *})

;; solve for missing arg1
(defn reverse-solve-arg1 [result op arg]
  ((op->arg1-missing op) result arg))

(def op->arg2-missing {"*" /, "+" -, "-" #(- %2 %1), "/" #(/ %2 %1)})

;; solve for missing arg2
(defn reverse-solve-arg2 [result op arg]
  ((op->arg2-missing op) result arg))

(defn reverse-solve-one [[values forms current value]]
  (let [[arg1 op arg2] (forms current)
        val1 (values arg1)
        val2 (values arg2)]
    (if val1
      [values forms arg2 (reverse-solve-arg2 value op val1)]
      [values forms arg1 (reverse-solve-arg1 value op val2)])))

(def root "root")
(def human "humn")

(defn part2 []
  (->> (parse-input input)
       ((fn [[values forms]] [(dissoc values human) forms]))
       (iterate reduce-one)
       (partition 2)
       (drop-while (fn [[s1 s2]] (not= s1 s2)))
       ffirst
       ((fn [[values forms]]
          (let [[next-root _ value] (forms root)]
            [values forms next-root (values value)])))
       (iterate reverse-solve-one)
       (drop-while (fn [[_ _ current _]] (not= current human)))
       first
       last))

;; part 1:  232974643455000
;; part 2:  3740214169961

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )