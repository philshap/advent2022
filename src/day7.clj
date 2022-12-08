(ns day7
  (:require [clojure.string :as str]))

(def input
  (-> "src/day7-input.txt"
      slurp
      str/split-lines))

;; Add the size to all directories on the path.
;; dirs is a map of path => total size
;; paths is a list of the current path and all path prefixes
(defn update-dirs [paths dirs size]
  (reduce (fn [dirs path]
            (update dirs path (fnil #(+ size %) 0)))
          dirs paths))

;; Run a cd or handle the output of the ls command.
;; cd is run by changing the path. The path is a vector (stack) of directory names.
;; For non-cd lines, if it starts with a number, add that to all directories on the path.
(defn run-command [[paths dirs] command]
  (let [[maybe-size cmd arg] (str/split command #" ")]
    (if (= cmd "cd")
      [(case arg
         "/" [["/"]]
         ".." (pop paths)
         (conj paths (conj (peek paths) arg)))
       dirs]
      [paths (update-dirs paths dirs
                         (or (parse-long maybe-size) 0))])))

(defn dir-sizes [input]
  (->> input
       (reduce run-command [[] {}])
       last))

(defn part1 []
  (->> input
       dir-sizes
       vals
       (filter #(< % 100000))
       (reduce +)))

(def total-space 70000000)
(def need-space 30000000)

(defn part2 []
  (let [raw-sizes (dir-sizes input)
        sizes (vals raw-sizes)
        root-size (raw-sizes ["/"])
        unused (- total-space root-size)]
    (->> sizes
         sort
         (drop-while #(< (+ unused %) need-space))
         first)))

; part 1:  1297159
; part 2:  3866390
(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )