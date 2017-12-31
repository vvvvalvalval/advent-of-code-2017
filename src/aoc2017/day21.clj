(ns aoc2017.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;--- Day 21: Fractal Art ---
;You find a program trying to generate some art. It uses a strange process that involves repeatedly enhancing the detail of an image through a set of rules.
;
;The image consists of a two-dimensional square grid of pixels that are either on (#) or off (.). The program always begins with this pattern:
;
;.#.
;..#
;###
;Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to have a size of 3.
;
;Then, the program repeats the following process:
;
;If the size is evenly divisible by 2, break the pixels up into 2x2 squares, and convert each 2x2 square into a 3x3 square by following the corresponding enhancement rule.
;Otherwise, the size is evenly divisible by 3; break the pixels up into 3x3 squares, and convert each 3x3 square into a 4x4 square by following the corresponding enhancement rule.
;Because each square of pixels is replaced by a larger one, the image gains pixels and so its size increases.
;
;The artist's book of enhancement rules is nearby (your puzzle input); however, it seems to be missing rules. The artist explains that sometimes, one must rotate or flip the input pattern to find a match. (Never rotate or flip the output pattern, though.) Each pattern is written concisely: rows are listed as single units, ordered top-down, and separated by slashes. For example, the following rules correspond to the adjacent patterns:
;
;../.#  =  ..
;          .#
;
;                .#.
;.#./..#/###  =  ..#
;                ###
;
;                        #..#
;#..#/..../#..#/.##.  =  ....
;                        #..#
;                        .##.
;When searching for a rule to use, rotate and flip the pattern as necessary. For example, all of the following patterns match the same rule:
;
;.#.   .#.   #..   ###
;..#   #..   #.#   ..#
;###   ###   ##.   .#.
;Suppose the book contained the following two rules:
;
;../.# => ##./#../...
;.#./..#/### => #..#/..../..../#..#
;As before, the program begins with this pattern:
;
;.#.
;..#
;###
;The size of the grid (3) is not divisible by 2, but it is divisible by 3. It divides evenly into a single square; the square matches the second rule, which produces:
;
;#..#
;....
;....
;#..#
;The size of this enhanced grid (4) is evenly divisible by 2, so that rule is used. It divides evenly into four squares:
;
;#.|.#
;..|..
;--+--
;..|..
;#.|.#
;Each of these squares matches the same rule (../.# => ##./#../...), three of which require some flipping and rotation to line up with the rule. The output for the rule is the same in all four cases:
;
;##.|##.
;#..|#..
;...|...
;---+---
;##.|##.
;#..|#..
;...|...
;Finally, the squares are joined into a new grid:
;
;##.##.
;#..#..
;......
;##.##.
;#..#..
;......
;Thus, after 2 iterations, the grid contains 12 pixels that are on.
;
;How many pixels stay on after 5 iterations?

(defn parse-pattern
  [ps]
  (->> (str/split ps #"/")
    (mapv vec)))

(defn rotate
  [p]
  (let [n (count p)]
    (into []
      (map (fn [i]
             (into []
               (map (fn [j]
                      (-> p (get j) (get (- n i 1)))))
               (range n))))
      (range n))))

(defn flip
  [p]
  (let [n (count p)]
    (into []
      (map (fn [i]
             (into []
               (map (fn [j]
                      (-> p (get i) (get (- n j 1)))))
               (range n))))
      (range n))))

(defn rotations
  [p]
  (->> p (iterate rotate) (take 4) set))

(defn isometries
  [p]
  (clojure.set/union
    (rotations p)
    (rotations (flip p))))

(comment
  (rotate
    [[1 0 0]
     [1 0 2]
     [0 1 0]])
  =>
  [[0 2 0]
   [0 0 1]
   [1 1 0]]
  )

(defn parse-rules
  [input]
  (->> input
    (str/split-lines)
    (map (fn [l]
           (let [[left right] (str/split l #"\s=>\s")]
             [(parse-pattern left) (parse-pattern right)])))
    (mapcat (fn [[left right]]
              (for [l (isometries left)]
                [l right])))
    (into {})))

(comment
  (def input (slurp (io/resource "aoc2017/day21.txt")))
  (def rules (parse-rules input))

  )

(defn subgrid [grid i0 j0 i1 j1]
  (->> (subvec grid i0 i1)
    (mapv #(subvec % j0 j1))))

(defn empty-grid [N]
  (into []
    (repeat N
      (into []
        (repeat N nil)))))

(defn enhance-grid
  [rules grid]
  (let [N (count grid)
        l (if (even? N) 2 3)]
    (->>
      (for [I (range (quot N l))
            J (range (quot N l))
            :let [sg (subgrid grid
                       (* I l) (* J l)
                       (+ (* I l) l) (+ (* J l) l))
                  enhanced (-> (get rules sg)
                             (or (throw (ex-info "No matching rule"
                                          {:sg sg :I I :J J}))))]
            i- (range (inc l))
            j- (range (inc l))
            :let [i (+ (* I (inc l)) i-)
                  j (+ (* J (inc l)) j-)]]
        [i j (-> enhanced (get i-) (get j-))])
      (reduce
        (fn [ret [i j v]]
          (update ret i #(assoc % j v)))
        (empty-grid (-> N (quot l) (* (inc l))))))))

(def p0
  [[\. \# \.]
   [\. \. \#]
   [\# \# \#]])

(defn count-on-pixels
  [grid]
  (->> grid
    (mapcat identity)
    (filter #{\#})
    count))

(defn solve1
  [rules]
  (count-on-pixels
    (nth
      (iterate #(enhance-grid rules %) p0)
      5)))

(comment
  (solve1 rules)
  => 179
  )




