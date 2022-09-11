(ns aoc.y2015.day03)

(set! *warn-on-reflection* true)

(defonce ^:private data (slurp "resources/2015/day03"))

(defn- parse-direction [ch]
  (case ch
    \v :north
    \^ :south
    \< :west
    \> :east))

(defn- parse-input [data]
  (map parse-direction data))

(defn- update-loc [[x y] dir]
  (case dir
    :north [x (inc y)]
    :south [x (dec y)]
    :east  [(inc x) y]
    :west  [(dec x) y]))

(defn- visit-houses [directions]
  (loop [loc [0 0]
         visited-houses (transient #{})
         dirs directions]
    (let [visited-houses (conj! visited-houses loc)]
      (if-let [s (seq dirs)]
        (recur (update-loc loc (first s))
               visited-houses
               (rest s))
        (persistent! visited-houses)))))

(defn- visit-houses-2 [directions]
  (loop [santa-loc [0 0]
         robo-santa-loc [0 0]
         visited-houses (transient #{})
         dirs directions]
    (let [visited-houses (-> visited-houses
                             (conj! santa-loc)
                             (conj! robo-santa-loc))]
      (if-let [[santa-dir robo-santa-dir & ds] dirs]
        (recur (update-loc santa-loc santa-dir)
               (update-loc robo-santa-loc robo-santa-dir)
               visited-houses
               ds)
        (persistent! visited-houses)))))

(defn puzzle-1 []
  (-> data
      parse-input
      visit-houses
      count))

(defn puzzle-2 []
  (-> data
      parse-input
      visit-houses-2
      count))

(comment
  (puzzle-1)
  2081
  (puzzle-2)
  2341)
