(ns aoc2017.day16
  (:require [clojure.java.io :as io]
            [aoc2017.utils :as u]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;--- Day 16: Permutation Promenade ---
;You come upon a very unusual sight; a group of programs here appear to be dancing.
;
;There are sixteen programs in total, named a through p. They start by standing in a line: a stands in position 0, b stands in position 1, and so on until p, which stands in position 15.
;
;The programs' dance consists of a sequence of dance moves:
;
;Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For example, s3 on abcde produces cdeab).
;Exchange, written xA/B, makes the programs at positions A and B swap places.
;Partner, written pA/B, makes the programs named A and B swap places.
;For example, with only five programs standing in a line (abcde), they could do the following dance:
;
;s1, a spin of size 1: eabcd.
;x3/4, swapping the last two programs: eabdc.
;pe/b, swapping programs e and b: baedc.
;After finishing their dance, the programs end up in order baedc.
;
;You watch the dance for a while and record their dance moves (your puzzle input). In what order are the programs standing after their dance?
;

(defn parse
  [input]
  )

(comment
  (def input (slurp (io/resource "aoc2017/day16.txt")))
  (def parsed (parse input))
  )

(defn parse
  [input]
  (u/parse-by
    [[:Spin #"s(\d+)" u/parse-long]
     [:Exchange #"x(\d+)/(\d+)" u/parse-long u/parse-long]
     [:Partner #"p([a-p])/([a-p])" symbol symbol]]
    (->> (str/split input #",") (str/join "\n"))))

;; ------------------------------------------------------------------------------
;; Part 1

(defmulti dance-move
  (fn [state [op]] op))

(defn spin
  [state x]
  (let [n (count state)
        o (mod (- x) n)]
    (into []
      (comp
        (drop o)
        (take n))
      (cycle state))))

(defmethod dance-move :Spin
  [state [_ x]]
  (spin state x))

(defn exchange
  [state A B]
  (assoc state
    A (get state B)
    B (get state A)))

(defmethod dance-move :Exchange
  [state [_ A B]]
  (exchange state A B))

(defn index-of
  [state p]
  (->> state
    (map-indexed (fn [i p] [i p]))
    (filter (fn [[i p1]] (= p1 p)))
    ffirst))

(defn partner
  [state A B]
  (let [iA (index-of state A)
        iB (index-of state B)]
    (exchange state iA iB)))

(defmethod dance-move :Partner
  [state [_ A B]]
  (partner state A B))

(def init-state
  (into [] (map (comp symbol str)) (seq "abcdefghijklmnop")))

(defn run-dance
  [state parsed]
  (reduce dance-move state parsed))

(defn solve1
  [parsed]
  (->> parsed
    (run-dance init-state)
    (map str)
    (apply str)))

(comment
  (require 'criterium.core)
  (let [parsed (parse input)]
    (criterium.core/quick-bench
      ;;  Execution time mean : 22.175221 ms
      (run-dance init-state parsed))))

;; ------------------------------------------------------------------------------
;; Part 2
;; running the whole dance is idempotent (Exists p such that dance^p = dance), because each move
;; of the dance consists of multiplying the state (seen as a permutation) with another permutation
;; (either a rotation or a transposition) on the left or on the right.
;; so there exist permutation P and Q such that dance(S) = P * S * Q; in particular, dance^n(S)=P^n * S * Q^n.
;; P and Q are permutations, so they are idempotent for some exponents p and q, and dance is idempotent of exponent (at most) p x q.
;; Therefore, we can count on dance being periodic, and on the period being probably relatively low (1e4, not 1e13, since the maximum
;; idempotence exponent for a 16-elements permutation is 5 * 4 * 140).

(defn find-period
  [parsed]
  (loop [state init-state
         i 0
         seen {init-state 0}]
    (let [next-state (run-dance state parsed)
          next-i (inc i)]
      (if-let [i0 (seen next-state)]
        (- next-i i0)
        (recur
          next-state
          next-i
          (assoc seen next-i next-state))))))

(defn solve2
  [parsed]
  (let [period (find-period parsed)
        k (int (mod 1e9 period))]
    (->> (nth
           (iterate #(run-dance % parsed) init-state)
           k)
      (map str)
      (apply str))))

(comment
  (solve2 parsed)
  => "abihnfkojcmegldp"
  )

;; ------------------------------------------------------------------------------
;; Bonus: let's have a bit of fun with 'AOT compilation', using some permutations algebra
;; we'll optimize our execution time by static analysis of the dance moves
;; we'll compress our whole input into 2 permutations: a permutation on programs, and a permutation on positions.

(defn compose-perms
  "composes 2 permutations, works on both map and vector representations"
  ([p1] p1)
  ([p1 p2]
   (persistent!
     (reduce-kv
       (fn [t x y]
         (assoc! t x (p1 y)))
       (transient p2) p2)))
  ([p1 p2 p3 & ps]
    (reduce compose-perms
      (compose-perms p1 p2)
      (cons p3 ps))))

(defn spin-perm
  [x]
  (->> (cycle (range 16))
    (drop (- 16 x))
    (take 16)
    vec))

(def id-indexes
  (vec (range 16)))

(defn exchange-perm
  [i j]
  (assoc id-indexes
    i j
    j i))

(def id-letters
  (let [letters (into [] (map (comp symbol str)) "abcdefghijklmnop")]
    (zipmap letters letters)))

(defn partner-perm
  [x y]
  (assoc id-letters
    x y
    y x))

(defn compile-dance
  [parsed]
  (loop [steps parsed
         perm-indexes id-indexes
         perm-letters id-letters]
    (if (empty? steps)
      (fn run-dance [state]
        (compose-perms perm-letters state perm-indexes))
      (let [[perm-indexes perm-letters]
            (match [(first steps)]
              [[:Spin x]]
              [(compose-perms perm-indexes (spin-perm x))
               perm-letters]

              [[:Exchange i j]]
              [(compose-perms perm-indexes (exchange-perm i j))
               perm-letters]

              [[:Partner x y]]
              [perm-indexes
               (compose-perms (partner-perm x y) perm-letters)])]
        (recur (next steps) perm-indexes perm-letters)))))

(comment
  ;; some benchmarking now!
  (require 'criterium.core)
  (let [parsed (parse input)
        c (compile-dance parsed)]
    (criterium.core/quick-bench
      ;; Execution time mean : 2.399018 µs, that is a 10000x improvement, using no Java interop.
      (c init-state)))
  )






