(ns aoc2017.core
  (:require [aoc2017.utils :as u]
            [clojure.string :as str]
            [com.rpl.specter :as sp]))

;; ------------------------------------------------------------------------------
;; Day 1: Inverse Captcha

(defn inverse-captcha
  [digits]
  (let [digits (into [] (map u/parse-long) digits)
        n (count digits)]
    (->> digits cycle
      (partition 2 1)
      (take n)
      (filter (fn [[d1 d2]] (= d1 d2)))
      (map first)
      (apply +))))

(comment
  (inverse-captcha "1122")
  (inverse-captcha "1111")
  (inverse-captcha "1234")
  (inverse-captcha "91212129")
  )

;; ------------------------------------------------------------------------------
;; Day 2: Corruption Checksum

(defn cc-parse
  [input]
  (->> input
    u/parse-table
    (sp/transform [sp/ALL sp/ALL] u/parse-long)))

(defn cc [input]
  (let [rows (cc-parse input)]
    (->> rows
      (map (fn [row]
             (let [mn (apply min row)
                   mx (apply max row)]
               (- mx mn))))
      (apply +))))

(comment
  (cc-parse "5 1 9 5\n7 5 3\n2 4 6 8")
  (cc "5 1 9 5\n7 5 3\n2 4 6 8")
  )



