(ns aoc.y2015.day06
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [instaparse.transform :as insta.t]))

(set! *warn-on-reflection* true)

(defonce ^:private data (slurp "resources/2015/day06"))

(def ^:private parser
  (insta/parser
   "S = COMMAND <WS> COORDINATE <WS> <'through'> <WS> COORDINATE
    COMMAND = 'turn off' | 'turn on' | 'toggle'
    COORDINATE = DIGIT <','> DIGIT
    WS = #'\\s+'
    <DIGIT> = #'[0-9]+'"))

(defn- parse-line [line]
  (insta.t/transform
   {:COORDINATE (fn [x y] [(parse-long x) (parse-long y)])
    :COMMAND (fn [com-str]
               (case com-str
                 "turn off" :turnoff
                 "turn on" :turnon
                 "toggle" :toggle))
    :S vector}
   (parser line)))

(defn- parse-data [data]
  (map parse-line (str/split-lines data)))

(defn- in-bounds? [[x y] [x-low y-low] [x-high y-high]]
  (and (<= x-low x x-high)
       (<= y-low y y-high)))

(defn- light-state [point instrs]
  (if-let [s (seq instrs)]
    (let [[com tr bl] (first s)]
      (if (in-bounds? point tr bl)
        (case com
          :turnon  true
          :turnoff false
          :toggle (not (light-state point (rest s))))
        (recur point (rest s))))
    false))

(defn- count-on-lights [instrs]
  (reduce (fn [c p]
            (if (light-state p instrs)
              (inc c)
              c))
          0
          (for [x (range 0 1000)
                y (range 0 1000)]
            [x y])))

(defn- puzzle-1 []
  (->> data
       parse-data
       reverse
       count-on-lights))

(defn- light-brightness [point instrs]
  (if-let [s (seq instrs)]
    (let [[com tr bl] (first s)]
      (if (in-bounds? point tr bl)
        (case com
          :turnon  (inc (light-brightness point (rest s)))
          :turnoff (max 0 (dec (light-brightness point (rest s))))
          :toggle  (+ 2 (light-brightness point (rest s))))
        (recur point (rest s))))
    0))

(defn- total-brightness [instrs]
  (reduce (fn [t p] (+ t (light-brightness p instrs)))
          0
          (for [x (range 0 1000)
                y (range 0 1000)]
            [x y])))

(defn- puzzle-2 []
  (->> data
       parse-data
       reverse
       total-brightness))

(comment
  (puzzle-1)
  400410
  (puzzle-2)
  15343601)
