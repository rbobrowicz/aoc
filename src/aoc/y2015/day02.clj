(ns aoc.y2015.day02
  (:require [instaparse.core :as insta]))

(set! *warn-on-reflection* true)

(defonce ^:private data (slurp "resources/2015/day02"))

(def ^:private parser
  (insta/parser
   "<S> = LINE*;
    LINE = DIGIT <'x'> DIGIT <'x'> DIGIT <'\n'>
    <DIGIT> = #'[0-9]+'"))

(defn- parse-data [data]
  (->> data
       parser
       (map (fn [[_ l w h]] [(parse-long l)
                             (parse-long w)
                             (parse-long h)]))))

(defn- calc-paper-needed [[l w h]]
  (let [side1-area (* l w)
        side2-area (* l h)
        side3-area (* w h)
        smallest-area (min side1-area
                           side2-area
                           side3-area)]
    (+ (* 2 side1-area)
       (* 2 side2-area)
       (* 2 side3-area)
       smallest-area)))

(defn- total-paper-needed [dims-seq]
  (reduce + 0 (map calc-paper-needed dims-seq)))

(defn puzzle-1 []
  (-> data
      parse-data
      total-paper-needed))

(defn- ribbon-wrap-length [[l w h]]
  (let [side1-perimeter (+ (* 2 l) (* 2 w))
        side2-perimeter (+ (* 2 l) (* 2 h))
        side3-perimeter (+ (* 2 w) (* 2 h))]
    (min side1-perimeter
         side2-perimeter
         side3-perimeter)))

(defn- ribbon-bow-length [[l w h]]
  (* l w h))

(defn- ribbon-length [dims]
  (+ (ribbon-wrap-length dims)
     (ribbon-bow-length dims)))

(defn- total-ribbon-needed [dims-seq]
  (reduce + 0 (map ribbon-length dims-seq)))

(defn puzzle-2 []
  (-> data
      parse-data
      total-ribbon-needed))

(comment
  (puzzle-1)
  1598415
  (puzzle-2)
  3812909)
