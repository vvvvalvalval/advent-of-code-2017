(ns aoc2017.utils
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn parse-long [x]
  (Long/parseLong (str x) 10))

(defn parse-table
  [input]
  (->> input
    (str/split-lines)
    (mapv #(str/split % #"\s+"))
    ))

(defn parse-by
  [tag+regex+coercers input]
  (->> (str/split-lines input)
    (map-indexed
      (fn [i line]
        (or
          (->> tag+regex+coercers
            (map (fn [[tag reg & coercers]]
                   (if-let [[_ & vs] (re-matches reg line)]
                     (into [tag]
                       (filter some?)
                       (map (fn [v f] (when f (f v)))
                         vs coercers)))))
            (filter some?)
            first)
          (throw (ex-info
                   (str "Failed to parse line " (pr-str i) ": " (pr-str line))
                   {:line line
                    :i i})))))))
