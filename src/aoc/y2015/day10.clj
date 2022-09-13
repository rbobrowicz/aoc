(ns aoc.y2015.day10)

(set! *warn-on-reflection* true)

(def ^:private input "1113222113")

(defn- look-and-say [s]
  (->> s
       (partition-by identity)
       (map (fn [x] (str (count x) (first x))))
       (apply str)))

(defn- puzzle-1 []
  (count
   (reduce (fn [s _] (look-and-say s)) input (range 40))))

(defn- puzzle-2 []
  (count
   (reduce (fn [s _] (look-and-say s)) input (range 50))))

(comment
  (puzzle-1)
  252594
  (puzzle-2)
  3579328)
