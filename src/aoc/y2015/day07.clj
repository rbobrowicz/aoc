(ns aoc.y2015.day07
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [instaparse.transform :as insta.t]))

(set! *warn-on-reflection* true)

(defonce ^:private data (slurp "resources/2015/day07"))

(def ^:private parser
  (insta/parser
   "S = SIGNAL | AND | OR | LSHIFT | RSHIFT | NOT
    SIGNAL = INPUT <ARROW> ID
    AND = INPUT <ANDLIT> INPUT <ARROW> ID
    OR = INPUT <ORLIT> INPUT <ARROW> ID
    LSHIFT = INPUT <LSHIFTLIT> DIGIT <ARROW> ID
    RSHIFT = INPUT <RSHIFTLIT> DIGIT <ARROW> ID
    NOT = <NOTLIT> INPUT <ARROW> ID
    <INPUT> = DIGIT | ID
    ANDLIT = WS 'AND' WS
    ORLIT = WS 'OR' WS
    LSHIFTLIT = WS 'LSHIFT' WS
    RSHIFTLIT = WS 'RSHIFT' WS
    NOTLIT = 'NOT' WS
    ARROW = WS '->' WS
    WS = #'\\s+'
    ID = #'[a-z]+'
    DIGIT = #'[0-9]+'"))

(defn- parse-line [line]
  (insta.t/transform
   {:S identity
    :ID keyword
    :DIGIT parse-long}
   (parser line)))

(defn- parse-data [data]
  (map parse-line (str/split-lines data)))

(defn- get-value [x circuit]
  (if (keyword? x)
    (get circuit x)
    x))

(defmulti process-instruction (fn [instr _] (first instr)))

(defmethod process-instruction :AND [[_ a b z] circuit]
  (let [a-val (get-value a circuit)
        b-val (get-value b circuit)]
    (when (and a-val b-val)
      (assoc circuit z (bit-and a-val b-val)))))

(defmethod process-instruction :OR [[_ a b z] circuit]
  (let [a-val (get-value a circuit)
        b-val (get-value b circuit)]
    (when (and a-val b-val)
      (assoc circuit z (bit-or a-val b-val)))))

(defmethod process-instruction :LSHIFT [[_ a n z] circuit]
  (let [a-val (get-value a circuit)]
    (when a-val
      (assoc circuit z (bit-and 0xFFFF (bit-shift-left a-val n))))))

(defmethod process-instruction :RSHIFT [[_ a n z] circuit]
  (let [a-val (get-value a circuit)]
    (when a-val
      (assoc circuit z (bit-shift-right a-val n)))))

(defmethod process-instruction :NOT [[_ a z] circuit]
  (let [a-val (get-value a circuit)]
    (when a-val
      (assoc circuit z (bit-and 0xFFFF (bit-not a-val))))))

(defmethod process-instruction :SIGNAL [[_ a z] circuit]
  (let [a-val (get-value a circuit)
        z-val (get-value z circuit)]
    (when (and a-val (nil? z-val))
      (assoc circuit z a-val))))

(defn- run-circuit [instrs]
  (loop [is instrs
         leftover-instructions nil
         circuit {}]
    (if-let [s (seq is)]
      (if-let [new-circuit (process-instruction (first s) circuit)]
        (recur (rest s) leftover-instructions new-circuit)
        (recur (rest s) (cons (first s) leftover-instructions) circuit))
      (if-let [a (get circuit :a)]
        a
        (recur leftover-instructions nil circuit)))))

(defn- puzzle-1 []
  (run-circuit (parse-data data)))

(defn- puzzle-2 []
  (run-circuit (cons [:SIGNAL 956 :b] (parse-data data))))

(comment
  (puzzle-1)
  956
  (puzzle-2)
  40149)
