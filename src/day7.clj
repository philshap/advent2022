(ns day7
  (:require [clojure.string :as str]))

(def input
  (-> "src/day7-input.txt"
      slurp
      str/split-lines))

;; Add the size to all directories on the path.
;; dirs is a map of path => total size
(defn update-dirs [path dirs size]
  (loop [path path
         dirs dirs]
    (if (empty? path)
      dirs
      (recur (pop path)
             (update dirs path (fnil #(+ size %) 0))))))

;; Run a cd or handle the output of the ls command.
;; cd is run by changing the path. The path is a vector (stack) of directory names.
;; For non-cd lines, if it starts with a number, add that to all directories on the path.
(defn run-command [[path dirs] command]
  (let [[maybe-size cmd arg] (str/split command #" ")]
    (if (= cmd "cd")
      [(case arg
         "/" ["/"]
         ".." (pop path)
         (conj path arg))
       dirs]
     (let [size (parse-long maybe-size)
           new-dirs (if (number? size)
                      (update-dirs path dirs size)
                      dirs)]
       [path new-dirs]))))

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
;(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  ;)