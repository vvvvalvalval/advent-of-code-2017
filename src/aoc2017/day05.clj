(ns aoc2017.day05)

(set! *warn-on-reflection* true)

(defn run
  [offsets]
  (let [^ints offsets (into-array Integer/TYPE offsets)]
    (loop [i 0
           t 0]
      (if (and (<= (int 0) i) (< i (alength offsets)))
        (let [o (aget offsets i)]
          (aset offsets i (inc o))
          (recur (+ i o) (inc t)))
        t))))

(def input)

(defn parse [input]
  (read-string (str "[" input "]")))

(def offsets (parse input))

(run offsets)

(defn run2
  [offsets]
  (let [^ints offsets (into-array Integer/TYPE offsets)]
    (loop [i 0
           t 0]
      (if (and (<= (int 0) i) (< i (alength offsets)))
        (let [o (aget offsets i)]
          (aset offsets i
            (if (> o (int 2))
              (dec o)
              (inc o)))
          (recur (+ i o) (inc t)))
        t))))
(time
  (run2 offsets))