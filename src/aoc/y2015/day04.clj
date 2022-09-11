(ns aoc.y2015.day04
  (:import [java.security MessageDigest]
           [java.nio.charset StandardCharsets]))

(set! *warn-on-reflection* true)

(defonce ^:private secret "iwrupvqb")

(defn- md5 [^String s]
  (let [md (MessageDigest/getInstance "MD5")]
    (.update md (.getBytes s StandardCharsets/US_ASCII))
    (.digest md)))

(defn- five-leading-zeros? [^bytes bs]
  (and (= 0 (aget bs 0))
       (= 0 (aget bs 1))
       (< (bit-and 0xFF (aget bs 2)) 128)))

(defn- mine [secret check-fn]
  (loop [i 1]
    (let [v (str secret i)
          hash (md5 v)]
      (if (check-fn hash)
        i
        (recur (inc i))))))

(defn- six-leading-zeros? [^bytes bs]
  (and (= 0 (aget bs 0))
       (= 0 (aget bs 1))
       (= 0 (aget bs 2))))

(defn puzzle-1 []
  (mine secret five-leading-zeros?))

(defn puzzle-2 []
  (mine secret six-leading-zeros?))

(comment
  (puzzle-1)
  346386
  (puzzle-2)
  9958218)
