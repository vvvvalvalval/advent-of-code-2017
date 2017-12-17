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

;; ------------------------------------------------------------------------------
;; Parsing

(defn parse
  [input]
  (u/parse-by
    [[:Spin #"s(\d+)" u/parse-long]
     [:Exchange #"x(\d+)/(\d+)" u/parse-long u/parse-long]
     [:Partner #"p([a-p])/([a-p])" symbol symbol]]
    (->> (str/split input #",") (str/join "\n"))))

(comment
  (def input (slurp (io/resource "aoc2017/day16.txt")))
  (def parsed (parse input))

  (take 5 parsed)
  => ([:Spin 2] [:Exchange 5 15] [:Partner f a] [:Exchange 12 10] [:Partner p h])
  )

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
;; ------------------------------------------------------------------------------
;; ------------------------------------------------------------------------------
;; BONUS: Using Permutations Algebra for fun and efficiency
(declare compose-perms)
;; We'll solve the problem with a different strategy, by noticing that the whole dance can be computed by composing the
;; positions of the dancers with 2 permutations.
;;
;; In particular, this will enable us to solve Part 2 without leveraging the cyclic nature of the dance at all,
;; because we'll have a fast algorithm for computing N repetitions of the dance, even if N is very large.
;;
;; A _Permutation_ is a one-to-one function of a finite set to itself.
;; In this program, we'll represent permutations as associative data structures:
;; - permutations on dancers will be represented as maps from dancer name to dancer name
;; - permutations on positions will be represented as vectors of numbers from 0 to 15
;;
;; The state of the dancers will be represented as a vector of symbols, for instance:
(comment
  ;; this is the initial state of the dancers
  [a b c d e f g h i j k l m n o p]

  ;; this is the dancers in some other state
  [c m p b a l f i d j k e g o h n])

;; Let's now see how dance moves translate to permutations.
;; *********************************
;; SPIN
;; - [:Spin k] consists of applying a circular permutation on the positions
(defn spin-perm
  "A permutation on the positions, that rotates them around."
  [k]
  (->> (cycle (range 16))
    (drop (- 16 k))
    (take 16)
    vec))

(comment
  (spin-perm 1)
  => [15 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14]
  (spin-perm 3)
  => [13 14 15 0 1 2 3 4 5 6 7 8 9 10 11 12])
;; this means we should compose this permutation on the right side of the dancers state:
(comment
  (compose-perms
    '[a b c d e f g h i j k l m n o p]
    (spin-perm 3))
  => [n o p a b c d e f g h i j k l m]

  (compose-perms
    '[c m p b a l f i d j k e g o h n]
    (spin-perm 3))
  => [o h n c m p b a l f i d j k e g])

;; *********************************
;; EXCHANGE
;; - [:Exchange i j] consists of applying a transposition (swapping 2 elements) on the positions:
(def id-positions
  "the identity permutation on positions"
  (vec (range 16)))

(defn exchange-perm
  [i j]
  (assoc id-positions
    i j
    j i))
(comment
  (exchange-perm 1 15)
  => [0 15 2 3 4 5 6 7 8 9 10 11 12 13 14 1]
  )
;; like before, since this is a permutation on the positions, we compose it on the right side:
(comment
  (compose-perms
    '[a b c d e f g h i j k l m n o p]
    (exchange-perm 1 15))
  => [a p c d e f g h i j k l m n o b]
  (compose-perms
    '[c m p b a l f i d j k e g o h n]
    (exchange-perm 1 15))
  => [c n p b a l f i d j k e g o h m]
  )

;; *********************************
;; PARTNER
;; [:Partner x y] also consists of applying a transposition, but on the dancers instead of the positions.
(def id-dancers
  "the identity permutation on programs"
  (let [dancers (into [] (map (comp symbol str)) "abcdefghijklmnop")]
    (zipmap dancers dancers)))

(defn partner-perm
  [x y]
  (assoc id-dancers
    x y
    y x))

;; *********************************
;; COMPOSING PERMUTATIONS
;; It is critical to notice that the composition of 2 permutations is still a permutation.
;; Since a dance move consists of composing some elementary permutation either on the left
;; or on the right of our dancers state, we can deduce that there exist P and Q such that
;;
;; dance(state) = P * state * Q,
;;
;; where:
;; - P is a permutation on the dancers
;; - Q is a permutation on the positions
;; - * denotes the composition operator. (concretely defined below)
(defn compose-perms
  "Composes functions represented by an associative data structures (maps or vectors).
  The concrete type of the result will be that of the right operand."
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

;; *********************************
;; DANCE EXPONENTIATION
;; In particular, we can leverage this to efficiently compute the repetition of the dance:
;;
;; dance^N(state) = P^N * state * Q^N,
;;
;; where dance^N(state) denotes dance(dance(... dance(state)))
;;
;; So all we need is a fast algorithm for computing the exponentation of a permutation.
;; Fortunately, there's a fast algorithm for computing exponentiation by any operator:
(defn exponentiate
  "Given an associative operation op (such as +, *, comp, matrix multiplication, ...)
  efficiently computes (apply op (repeat n v)).
  The number of calls to `op` is O(log(n))."
  [op v n]
  (cond
    (> n 1)
    (let [v-half (exponentiate op v (quot n 2))]
      (cond-> (op v-half v-half)
        (odd? n)
        (op v)))

    (= n 1)
    v

    (= n 0)
    (op)))

(comment
  (exponentiate * 2 8)
  => 256
  (exponentiate * 2 10)
  => 1024
  (exponentiate + 5 100)
  => 500
  )

;; ------------------------------------------------------------------------------
;; Implementation

(defn dance-decomposition
  "Computes a representation of the whole dance as a pair of permutations:
  - :left : a permutation on the set of programs
  - :right : a permutation on the set of indices"
  [steps]
  {:left (->> steps
           (filter (fn [[op & _]] (#{:Partner} op)))
           (map (fn [step]
                  (match [step]
                    [[:Partner x y]] (partner-perm x y))))
           (reduce (fn [left perm] (compose-perms perm left)) id-dancers))
   :right (->> steps
            (filter (fn [[op & _]] (#{:Exchange :Spin} op)))
            (map (fn [step]
                   (match [step]
                     [[:Exchange i j]] (exchange-perm i j)
                     [[:Spin x]] (spin-perm x))))
            (reduce (fn [right perm] (compose-perms right perm)) id-positions))})

(defn dance-n-times
  [dance-decomp init-state N]
  (let [{:keys [left right]} dance-decomp]
    (compose-perms
      (exponentiate compose-perms left N)
      init-state
      (exponentiate compose-perms right N))))

(defn solve1'
  "Computes the state of the dancers after 1 dance"
  [parsed]
  (->> (dance-n-times (dance-decomposition parsed) init-state 1)
    (apply str)))

(defn solve2'
  "Computes the state of the dancers after 1 billion dances"
  [parsed]
  (->> (dance-n-times (dance-decomposition parsed) init-state 1000000000)
    (apply str)))


(comment
  ;; some benchmarking now!
  (require 'criterium.core)

  (criterium.core/bench
    (solve1' parsed))
  ;Evaluation count : 1860 in 60 samples of 31 calls.
  ;           Execution time mean : 30.439864 ms
  ;  Execution time std-deviation : 2.805820 ms
  ; Execution time lower quantile : 25.621449 ms ( 2.5%)
  ; Execution time upper quantile : 35.170986 ms (97.5%)
  ;                 Overhead used : 2.303380 ns

  (criterium.core/bench
    ;; Execution time mean : 29.728498 ms
    (solve2' parsed))
  ;Evaluation count : 2040 in 60 samples of 34 calls.
  ;           Execution time mean : 30.915240 ms
  ;  Execution time std-deviation : 2.301706 ms
  ; Execution time lower quantile : 26.898057 ms ( 2.5%)
  ; Execution time upper quantile : 34.771747 ms (97.5%)
  ;                 Overhead used : 2.303380 ns

  ;; So there's virtually no performance difference between running the dance once
  ;; or running it a billion times.
  ;; The dominant term in the computation is the one-time cost of compiling the dance moves into
  ;; the permutations representation.
  )






