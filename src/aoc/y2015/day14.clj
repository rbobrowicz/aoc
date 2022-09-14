(ns aoc.y2015.day14
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [instaparse.transform :as insta.t]))

(set! *warn-on-reflection* true)

(defonce ^:private data (slurp "resources/2015/day14"))

(def ^:private parser
  (insta/parser
   "S = NAME <CANFLY> SPEED <FOR> DURATION <','> <REST-TXT> DURATION <'.'>
    NAME = #'[a-zA-Z]+'
    CANFLY = WS 'can fly' WS
    SPEED = #'[0-9]+' <WS> <'km/s'>
    FOR = WS 'for' WS
    DURATION = #'[0-9]+' <WS> <'seconds'>
    REST-TXT = WS 'but then must rest for' WS
    WS = #'\\s'+ "))

(defn- parse-line [line]
  (insta.t/transform
   {:DURATION parse-long
    :SPEED parse-long
    :NAME keyword
    :S vector}
   (parser line)))

(defn- parse-data [data]
  (map parse-line (str/split-lines data)))

(defn- race-raindeer [time raindeer]
  (let [[_ speed run-time rest-time] raindeer
        cycle-time (+ run-time rest-time)
        full-cycles (quot time cycle-time)
        rem-time (rem time cycle-time)]
    (+ (* speed run-time full-cycles)
       (* speed (min run-time rem-time)))))

(defn- race-raindeers [time raindeers]
  (map (fn [r] [(first r) (race-raindeer time r)]) raindeers))

(defn puzzle-1 []
  (->> data
       parse-data
       (race-raindeers 2503)
       (map second)
       (apply max)))

(defn- leads [results]
  (let [lead-distance (apply max (map second results))]
    (->> results
         (filter #(= (second %) lead-distance))
         (map first))))

(defn- race-tick [time raindeers standings]
  (loop [leads (leads (race-raindeers time raindeers))
         standings standings]
    (if-let [s (seq leads)]
      (let [r (first s)
            curr-score (get standings r 0)]
        (recur (rest s)
               (assoc! standings r (inc curr-score))))
      standings)))

(defn- run-race [time-limit raindeer]
  (persistent!
   (reduce (fn [s t] (race-tick t raindeer s))
           (transient {})
           (range 1 (inc time-limit)))))

(defn- puzzle-2 []
  (->> data
       parse-data
       (run-race 2503)
       vals
       (apply max)))

(comment
  (puzzle-1)
  2660
  (puzzle-2)
  1256)
