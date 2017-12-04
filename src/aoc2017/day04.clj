(ns aoc2017.day04
  (:require [clojure.string :as str]))


(defn valid? [pw]
  (->> (str/split pw #"\s+")
    (apply distinct?)))

(defn solve1
  [input]
  (->> input
    (str/split-lines)
    (filter valid?)
    (count)))

(defn valid2? [pw]
  (->> (str/split pw #"\s+")
    (map frequencies)
    (apply distinct?)))

(defn solve2
  [input]
  (->> input
    (str/split-lines)
    (filter valid2?)
    (count)))

