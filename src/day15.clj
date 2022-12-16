(ns day15)

(defn distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(def input
  (->> (slurp "src/day15-input.txt")
       (re-seq #"-?\d+")
       (map parse-long)
       (partition 2)
       (partition 2)
       (map (fn [[sensor beacon]] [sensor beacon (distance sensor beacon)]))))

(defn row-range [row [[x y] _ distance]]
  (if (< (- y distance) row (+ y distance))
    (let [offset (- distance (abs (- y row)))]
      [(- x offset) (+ x offset)])))

(defn merge-ranges [ranges [range-min range-max :as row-range]]
  (let [prev (last ranges)]
    (if (or (empty? prev) (< (second prev) range-min))
      (conj ranges row-range)
      (conj (subvec ranges 0 (dec (count ranges)))
            [(first prev) (max (second prev) range-max)]))))

(defn merged-ranges-at-row [sensors at-row]
  (->> sensors
       (map (partial row-range at-row))
       (filter some?)
       (sort-by first)
       (reduce merge-ranges [])))
(defn beacons-on-row [row sensors]
  (->> sensors
       (map second)
       (filter (fn [[_ y]] (= y row)))
       distinct
       count))

(def row-check 2000000)
(defn part1 []
  (->> (merged-ranges-at-row input row-check)
       (map (fn [[low hi]] (inc (- hi low))))
       (reduce +)
       (#(- % (beacons-on-row row-check input)))))

(def max-distance 4000000)

;; This works from 0, but it takes a long time. Start it just before the solution to cheat.
(def cheat 2625400)
(defn part2 []
  (->> (range 0 max-distance)
       (map (juxt identity (partial merged-ranges-at-row input)))
       (filter (fn [[_ ranges]] (= (count ranges) 2)))
       first
       ((fn [[y [[_ x]]]] (+ (* x max-distance) y)))))

;; part 1:  4582667
;; part 2:  10961114625406

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )