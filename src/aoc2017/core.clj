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

(defn ic2
  [digits]
  (let [digits (into [] (map u/parse-long) digits)
        n (count digits)
        cy (cycle digits)]
    (->> (map vector
           cy
           (drop (quot n 2) cy))
      (take n)
      (filter (fn [[d1 d2]] (= d1 d2)))
      (map first)
      (apply +))))

(comment
  (ic2 "1212")
  (ic2 "1221")
  (ic2 "123425")
  (ic2 "123123")
  (ic2 "12131415")
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

;; part 2

(defn divides? [num div]
  (zero? (rem num div)))

(defn cc2 [input]
  (let [rows (cc-parse input)]
    (->> rows
      (map (fn [row]
             (->>
               (for [num row
                     div row
                     :when (not= num div)
                     :when (divides? num div)]
                 (quot num div))
               first)))
      (apply +))))

(comment to-array
  (cc2 "5 9 2 8\n9 4 7 3\n3 8 6 5")
  )

