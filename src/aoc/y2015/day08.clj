(ns aoc.y2015.day08
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defonce ^:private data (slurp "resources/2015/day08"))

(defn- count-code-chars [s]
  (count s))

(defn- count-memory-chars [s]
  (loop [s (butlast (rest s))
         c 0]
    (if-let [s (seq s)]
      (case (first s)
        \\ (case (nth s 1)
             \\ (recur (nthrest s 2) (inc c))
             \" (recur (nthrest s 2) (inc c))
             \x (recur (nthrest s 4) (inc c)))
        (recur (rest s) (inc c)))
      c)))

(defn puzzle-1 []
  (let [[code-char-count mem-char-count]
        (reduce (fn [[c m] l] [(+ c (count-code-chars l))
                               (+ m (count-memory-chars l))])
                [0 0]
                (str/split-lines data))]
    (- code-char-count mem-char-count)))

(defn- count-escaped-chars [s]
  (+ 2 (loop [s s
              c 0]
         (if-let [s (seq s)]
           (case (first s)
             \\ (recur (rest s) (+ 2 c))
             \" (recur (rest s) (+ 2 c))
             (recur (rest s) (inc c)))
           c))))

(defn puzzle-2 []
  (let [[code-char-count esc-char-count]
        (reduce (fn [[c e] l] [(+ c (count-code-chars l))
                               (+ e (count-escaped-chars l))])
                [0 0]
                (str/split-lines data))]
    (- esc-char-count code-char-count)))

(comment
  (puzzle-1)
  1371
  (puzzle-2)
  2117)
