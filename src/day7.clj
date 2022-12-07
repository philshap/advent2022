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

;; For ls, consume lines until a new command line ($) or no more commands are present.
;; For each line, if it starts with a number, add that to all directories on the path.
;; "dir" lines are ignored
(defn run-ls [state]
  (->> state
       (iterate
         (fn [[[command & cmds] path dirs]]
           (let [size (parse-long (first (str/split command #" ")))
                 new-dirs (if (number? size)
                            (update-dirs path dirs size)
                            dirs)]
             [cmds path new-dirs])))
       (drop-while (fn [[[command & _] & _]]
                     (and (string? command)
                          (not (str/starts-with? command "$")))))
       first))

;; Run a cd or ls command.
;; cd is run by changing the path
(defn run-command [[[command & cmds] path dirs]]
  (if (string? command)
    (if (str/starts-with? command "$ cd")
      (let [arg (last (str/split command #" "))
            new-path (case arg
                       "/" ["/"]
                       ".." (pop path)
                       (conj path arg))]
        [cmds new-path dirs])
      (run-ls [cmds path dirs]))
    [nil path dirs]))

(defn dir-sizes [input]
  (->> [input [] {}]
       (iterate run-command)
       (drop-while #(not (empty? (first %))))
       first
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