(ns aoc.y2015.day13
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [instaparse.transform :as insta.t]))

(set! *warn-on-reflection* true)

(defonce ^:private data (slurp "resources/2015/day13"))

(def ^:private parser
  (insta/parser
   "S = NAME <WOULD> (GAIN | LOSE) <MIDSTUFF> NAME <'.'>
    NAME = #'[a-zA-Z]+'
    WOULD = WS 'would' WS
    GAIN = <'gain'> <WS> AMOUNT <WS>
    LOSE = <'lose'> <WS> AMOUNT <WS>
    AMOUNT = #'[0-9]+'
    MIDSTUFF = 'happiness units by sitting next to' WS
    WS = #'\\s'+ "))

(defn- parse-line [line]
  (insta.t/transform
   {:AMOUNT parse-long
    :GAIN identity
    :LOSE #(* -1 %)
    :NAME keyword
    :S (fn [sub gain-loss ob] [[sub ob] gain-loss])}
   (parser line)))

(defn parse-data [data]
  (into {} (map parse-line (str/split-lines data))))

(defn- unique-people [data]
  (apply set/union
         (map (fn [[[sub ob] _]] #{sub ob})
              data)))

(defn- paths [people]
  (if (empty? people)
    [[]]
    (loop [ps people
           accum []]
      (if-let [s (seq ps)]
        (let [curr-person (first s)
              other-people (disj people curr-person)
              sub-paths (paths other-people)
              new-paths (map #(conj % curr-person) sub-paths)]
          (recur (disj ps curr-person) (conj accum new-paths)))
        (mapcat identity accum)))))

(defn- make-path-circular [p]
  (when-let [s (seq p)]
    (conj p (first s))))

(defn- sum-happiness [happiness-map path]
  (->> path
       (partition 2 1)
       (map (fn [[x y]]
              (+ (get happiness-map [x y])
                 (get happiness-map [y x]))))
       (reduce + 0)))

(defn- add-santa [data]
  (let [people (unique-people data)]
    (-> data
        (into (map (fn [p] [[:santa p] 0]) people))
        (into (map (fn [p] [[p :santa] 0]) people)))))

(defn puzzle-1 [data]
  (let [people (unique-people data)
        circ-paths (map make-path-circular (paths people))]
    (apply max (map #(sum-happiness data %) circ-paths))))

(defn puzzle-2 [data]
  (let [data-with-santa (add-santa data)]
    (puzzle-1 data-with-santa)))

(comment
  (puzzle-1 (parse-data data))
  733
  (puzzle-2 (parse-data data))
  725)
