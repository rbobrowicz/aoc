(ns aoc.y2015.day09
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [instaparse.transform :as insta.t]))

(set! *warn-on-reflection* true)

(defonce ^:private data (slurp "resources/2015/day09"))

(def ^:private parser
  (insta/parser
   "S = PLACE <'to'> PLACE <'='> DISTANCE
    PLACE = <WS?> ID <WS?>
    <ID> = #'[a-zA-Z]+'
    DISTANCE = <WS?> #'[0-9]+' <WS?>
    WS = #'\\s'+ "))

(defn- parse-line [line]
  (insta.t/transform
   {:PLACE keyword
    :DISTANCE parse-long
    :S (fn [src dst dist] {[src dst] dist
                           [dst src] dist})}
   (parser line)))

(defn parse-data [data]
  (into {} (map parse-line (str/split-lines data))))

(defn- unique-cities [distances]
  (apply set/union
         (map (fn [[[src dst] _]] #{src dst})
              distances)))

(defn- paths [cities]
  (if (empty? cities)
    [[]]
    (loop [cs cities
           accum []]
      (if-let [s (seq cs)]
        (let [curr-city (first s)
              other-cities (disj cities curr-city)
              sub-paths (paths other-cities)
              new-paths (map #(conj % curr-city) sub-paths)]
          (recur (disj cs curr-city) (conj accum new-paths)))
        (mapcat identity accum)))))

(defn- path-distance [path distances]
  (loop [path path
         sum 0]
    (if-let [s (seq path)]
      (let [c1 (first s)
            c2 (second s)
            distance (long (get distances [c1 c2] 0))]
        (recur (rest s) (+ sum distance)))
      sum)))

;; There is an optimization here to cache the computed path
;; lengths and check for the reverse of each path. That would cut
;; the number of path distance computations by half, but not actually
;; change the asymptotic complexity. It's would still be O(n!) so
;; I'm not going to bother for this excercise since it runs in less
;; than a second anyway.

(defn puzzle-1 []
  (let [distances (parse-data data)
        cities (unique-cities distances)
        all-paths (paths cities)
        all-lengths (map #(path-distance % distances) all-paths)]
    (apply min all-lengths)))

(defn puzzle-2 []
  (let [distances (parse-data data)
        cities (unique-cities distances)
        all-paths (paths cities)
        all-lengths (map #(path-distance % distances) all-paths)]
    (apply max all-lengths)))

(comment
  (puzzle-1)
  141
  (puzzle-2)
  736)
