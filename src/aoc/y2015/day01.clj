(ns aoc.y2015.day01)

(defonce ^:private data (slurp "resources/2015/day01"))

(defn- floor->int [floor]
  (case floor
    \( 1
    \) -1))

(defn- follow-directions [dirs]
  (->> dirs
       (map floor->int)
       (reduce + 0)))

(defn puzzle-1 []
  (follow-directions data))

(defn- index-of-basement-instruction [dirs]
  (->> dirs
       (map floor->int)
       (reduce (fn [[sum idx] x]
                 (let [new-sum (+ sum x)]
                   (if (< new-sum 0)
                     (reduced idx)
                     [new-sum (inc idx)])))
               [0 1])))

(defn puzzle-2 []
  (index-of-basement-instruction data))

(comment
  (puzzle-1)
  74
  (puzzle-2)
  1795)
