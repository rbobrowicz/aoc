(ns aoc.y2015.day05
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defonce ^:private data (slurp "resources/2015/day05"))

(defonce ^:private vowels #{\a \e \i \o \u})

(defn- num-vowels [s]
  (count (filter vowels s)))

(defn- invalid-pair? [x y]
  (or (and (= x \a)
           (= y \b))
      (and (= x \c)
           (= y \d))
      (and (= x \p)
           (= y \q))
      (and (= x \x)
           (= y \y))))

(defn- nice-char-pairs? [s]
  (loop [pairs (partition 2 1 s)
         has-double false]
    (if-let [s (seq pairs)]
      (let [[x y] (first s)]
        (if (invalid-pair? x y)
          false
          (if (= x y)
            (recur (rest s) true)
            (recur (rest s) has-double))))
      has-double)))

(defn- nice? [s]
  (and (< 2 (num-vowels s))
       (nice-char-pairs? s)))

(defn puzzle-1 []
  (count (filter nice? (str/split-lines data))))

(defn- has-nice-triple? [s]
  (reduce (fn [_ [x _ z]] (if (= x z) (reduced true) false))
          false
          (partition 3 1 s)))

(defn- has-double-pair? [s]
  (loop [pairs (partition 2 1 s)
         seen-pairs (transient {})
         i 0]
    (when-let [s (seq pairs)]
      (if-let [j (get seen-pairs (first s))]
        (if (> (- i j) 1)
          true
          (recur (rest s)
                 seen-pairs
                 (inc i)))
        (recur (rest s)
               (assoc! seen-pairs (first s) i)
               (inc i))))))

(defn- nice2? [s]
  (and (has-nice-triple? s)
       (has-double-pair? s)))

(defn puzzle-2 []
  (count (filter nice2? (str/split-lines data))))

(comment
  (puzzle-1)
  236
  (puzzle-2)
  51)
