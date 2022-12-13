(ns day13
  (:require
    [clojure.edn :as edn]
    [clojure.string :as str]))

(def input
  (-> "src/day13-input.txt"
      slurp
      (str/split #"\n\n")
      (->> (map #(map edn/read-string (str/split % #"\n"))))))

(defn ordered? [left right]
  (cond
    (and (number? left) (number? right)) (if (= left right) :unknown (< left right))
    (and (vector? left) (vector? right))
    (loop [left left
           right right]
      (cond
        (empty? left) (if (empty? right) :unknown true)
        (empty? right) false
        :else (let [inner (ordered? (first left) (first right))]
                (if (= inner :unknown)
                  (recur (rest left) (rest right))
                  inner))))
    (number? left) (recur [left] right)
    (number? right) (recur left [right])))

(defn part1 []
  (->> input
       (keep-indexed
         (fn [index [left right]]
           (if (ordered? left right) index)))
       (map inc)
       (reduce +)))

(def div-packets #{[[2]] [[6]]})
(defn part2 []
  (->> (apply concat input)
       (concat div-packets)
       (sort ordered?)
       (keep-indexed #(if (div-packets %2) %1))
       (map inc)
       (reduce *)))

; part 1:  5390
; part 2:  19261

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )