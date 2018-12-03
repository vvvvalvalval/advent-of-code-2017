(ns aoc2018.day02
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn parse-input
  [raw-input]
  (->>
    (edn/read-string (str "[" raw-input "]"))
    (mapv str)))

(defn get-raw-input
  []
  (slurp (io/resource "aoc2018/day02.txt")))

(comment
  (def pi (parse-input (get-raw-input)))
  )

(defn letter-frequencies
  "Given a box id `box-id`, returns the set of integers n such that
  box-id countains a letter repeated exactly n times."
  [box-id]
  (->> box-id seq frequencies vals set))

(comment
  (letter-frequencies "bababc") => #{1 3 2}
  (letter-frequencies "abcdef")
  (letter-frequencies "abcccd")
  (letter-frequencies "aabcdd") => #{1 2}
  (letter-frequencies "ababab") => #{3}
  )

(defn frequencies-frequencies
  "Given the list of box ids, returns a map mapping each letter frequency `n`
  to the number `m` of box ids that contain a letter repeated exactly n times,
  where both n and m are positive."
  [parsed-input]
  (->> parsed-input
    (mapcat letter-frequencies)
    frequencies
    ))

(comment
  (frequencies-frequencies
    ["abcdef"
     "bababc"
     "abbcde"
     "abcccd"
     "aabcdd"
     "abcdee"
     "ababab"])
  => {1 6, 3 3, 2 4}
  )

(defn checksum
  [parsed-input]
  (let [ff (frequencies-frequencies parsed-input)]
    (*
      (get ff 2 0)
      (get ff 3 0))))

(comment
  (checksum ["abcdef"
             "bababc"
             "abbcde"
             "abcccd"
             "aabcdd"
             "abcdee"
             "ababab"])
  => 12
  )

(defn solve1
  [parsed-input]
  (checksum parsed-input))

(comment
  (solve1 pi))

;; ------------------------------------------------------------------------------
;; Part 2

(comment
  (Math/pow (count pi) 2) => 63500.0                        ;; Cool, likely not to big for a naive quadratic algorithm
  )

(defn differing-char-indexes
  [^String s1, ^String s2]
  (into []
    (filter
      (fn [i]
        (not=
          (.charAt s1 (int i))
          (.charAt s2 (int i)))))
    (range (.length s1))))

(comment
  (differing-char-indexes "abcde" "axcye") => [1 3]
  (differing-char-indexes "abcde" "abcde") => []
  )

(defn pairs
  [coll]
  (let [c (vec coll)
        N (count c)]
    (for [i (range N)
          j (range (inc i) N)
          :let [e1 (nth c i)
                e2 (nth c j)]]
      #{e1 e2})))

(defn differing-by-one
  [parsed-input]
  (for [p (pairs parsed-input)
        :let [[box-id-1 box-id-2] (seq p)
              diff-idxs (differing-char-indexes box-id-1 box-id-2)]
        :when (= 1 (count diff-idxs))]
    [box-id-1 box-id-2 (first diff-idxs)]))

(comment
  (differing-by-one ["abcdef"
                     "bababc"
                     "abbcde"
                     "abcccd"
                     "aabcdd"
                     "abcdee"
                     "ababab"])
  => (["abcdef" "abcdee" 5])
  )

(defn splice-at
  [^String s, i]
  {:pre [(<= 0 i (dec (.length s)))]}
  (str (.substring s 0 i) (.substring s (inc i) (.length s))))

(comment
  (splice-at "abcd" 0) => "bcd"
  (splice-at "abcd" 1) => "acd"
  (splice-at "abcd" 3) => "abc"
  )

(defn solve2
  [parsed-input]
  (let [diff1 (differing-by-one parsed-input)]
    (assert (= 1 (count diff1)))
    (let [[^String id1, _id2 diff-idx] (first diff1)]
      (splice-at id1 diff-idx))))

(comment
  (solve2 pi)
  )






