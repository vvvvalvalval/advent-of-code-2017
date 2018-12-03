(ns aoc2018.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-input
  [raw-input]
  (->> raw-input
    (str/split-lines)
    (mapv
      (fn [l]
        (let [[id x y w h]
              (edn/read-string
                (str
                  "["
                  (str/replace l #"\D" " ")
                  "]"))]
          [id x y w h])))))

(defn get-raw-input
  []
  (slurp (io/resource "aoc2018/day03.txt")))

(comment
  (def pi (parse-input (get-raw-input)))
  )

(defn pixels
  [claim]
  (let [[_id x y w h] claim]
    (for [i (range x (+ x w))
          j (range y (+ y h))]
      [i j])))

(defn pixel-freqs
  "Maps each pixel to the number of claims covering it"
  [parsed-input]
  (->> parsed-input
    (mapcat pixels)
    frequencies))

(comment
  (count (pixel-freqs pi)) => 350158                        ;; Cool, not that many pixels
  )

(defn n-overlapping-pixels
  [parsed-input]
  (let [pf (pixel-freqs parsed-input)]
    (->> pf
      vals
      (filter (fn [n] (> n 1)))
      count)))

(defn solve1
  [parsed-input]
  (n-overlapping-pixels parsed-input))

(comment
  (solve1 pi)
  )

(defn non-overlapping-claim?
  [pf claim]
  (every?
    (fn [px] (= (get pf px 0) 1))
    (pixels claim)))

(defn non-overlapping-claims
  [parsed-input]
  (let [pf (pixel-freqs parsed-input)]
    (filter #(non-overlapping-claim? pf %) parsed-input)))

(comment
  (non-overlapping-claims pi)
  )

(defn solve2
  [parsed-input]
  (let [nocs (non-overlapping-claims parsed-input)]
    (assert (= 1 (count nocs)))
    (let [[claim-id] (first nocs)]
      claim-id)))

(comment
  (solve2 pi)
  )



