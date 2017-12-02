(ns aoc2017.utils
  (require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn parse-long [x]
  (Long/parseLong (str x) 10))

(defn parse-table
  [input]
  (->> input
    (str/split-lines)
    (mapv #(str/split % #"\s+"))
    ))
