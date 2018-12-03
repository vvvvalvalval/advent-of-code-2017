(ns aoc2018.day01
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn get-input
  []
  (slurp (io/resource "aoc2018/day01.txt")))

(defn parse-input
  [input]
  (edn/read-string
    (str "[" input "]")))

(comment
  (def parsed-input
    (-> (get-input) parse-input))
  )

(defn solve1
  [parsed-input]
  (reduce + 0 parsed-input))

(comment
  (solve1 parsed-input)
  )

(defn first-repeated
  [coll]
  (loop [seen #{}
         c coll]
    (if (empty? c)
      (throw (ex-info "No repetitions" {}))
      (let [e (first c)]
        (if (contains? seen e)
          e
          (recur (conj seen e) (next c)))))))

(comment
  (first-repeated [0 2 -4 -2 2 0 5]) => 2
  )

(defn solve2
  [parsed-input]
  (first-repeated (reductions + 0 (cycle parsed-input))))

(comment
  (solve2 parsed-input)
  )


