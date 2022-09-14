(ns aoc.y2015.day11)

(set! *warn-on-reflection* true)

(def ^:private input "vzbxkghb")

;; This solution does a lot of direct
;; low level byte manipulation. I originally
;; had a more clojure-y solution, but it was
;; noticably slow, taking over a second compared
;; to this, which does a double round on the
;; puzzle input in 20-30 ms on my machine.

(defn- password->bytes ^bytes [p]
  (byte-array (map byte p)))

(defn- has-straight? [^bytes bs]
  (let [limit (- (alength bs) 2)]
    (loop [i 0]
      (when (< i limit)
        (let [a (aget bs i)
              b (aget bs (inc i))
              c (aget bs (inc (inc i)))]
          (if (and (= (inc a) b)
                   (= (inc b) c))
            true
            (recur (inc i))))))))

(defn- has-pairs? [^bytes bs]
  (let [limit (dec (alength bs))]
    (loop [i 0
           prev-pair (byte 0)]
      (if (< i limit)
        (if (= (aget bs i)
               (aget bs (inc i)))
          (if (< 0 prev-pair)
            (if (= prev-pair (aget bs i))
              (recur (inc i) prev-pair)
              true)
            (recur (inc i) (aget bs i)))
          (recur (inc i) prev-pair))
        false))))

(defn- valid-password? [^bytes p]
  (and (has-pairs? p)
       (has-straight? p)))

(defn- increment-password [^bytes p]
  (loop [i (dec (alength p))]
    (if (= (aget p i) (byte \z))
      (do
        (aset p i (byte \a))
        (recur (dec i)))
      (do
        (aset p i (->> i (aget p) inc byte))
        (case (aget p i)
          \l (recur i)
          \i (recur i)
          \o (recur i)
          p)))))

(defn- next-valid-password [^bytes p]
  (increment-password p)
  (if (valid-password? p)
    p
    (recur p)))

(defn- bytes->password [^bytes p]
  (apply str (map char p)))

(defn puzzle [pass]
  (-> pass
      password->bytes
      next-valid-password
      bytes->password))

(comment
  (puzzle input)
  "vzbxxyzz"
  (puzzle (puzzle input))
  "vzcaabcc")
