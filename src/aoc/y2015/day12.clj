(ns aoc.y2015.day12
  (:require [jsonista.core :as j]))

(set! *warn-on-reflection* true)

(defonce ^:private data (slurp "resources/2015/day12"))

(defn- eval-sum [json]
  (cond
    (vector? json)
    (reduce + 0 (map eval-sum json))

    (map? json)
    (reduce + 0 (map (fn [[_ v]] (eval-sum v)) json))

    (string? json)
    0

    (int? json)
    json))

(defn puzzle-1 []
  (eval-sum (j/read-value data)))

(defn- eval-sum-2 [json]
  (cond
    (vector? json)
    (reduce + 0 (map eval-sum-2 json))

    (map? json)
    (if (some #(= "red" %) (vals json))
      0
      (reduce + 0 (map (fn [[_ v]] (eval-sum-2 v)) json)))

    (string? json)
    0

    (int? json)
    json))

(defn puzzle-2 []
  (eval-sum-2 (j/read-value data)))

(comment
  (puzzle-1)
  156366
  (puzzle-2)
  96852)
