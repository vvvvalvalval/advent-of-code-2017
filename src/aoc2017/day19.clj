(ns aoc2017.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;--- Day 19: A Series of Tubes ---
;Somehow, a network packet got lost and ended up here. It's trying to follow a routing diagram (your puzzle input),
;but it's confused about where to go.
;
;Its starting point is just off the top of the diagram. Lines (drawn with |, -, and +) show the path it needs to take,
;starting by going down onto the only line connected to the top of the diagram. It needs to follow this path until
;it reaches the end (located somewhere within the diagram) and stop there.
;
;Sometimes, the lines cross over each other; in these cases, it needs to continue going the same direction, and only turn
; left or right when there's no other option. In addition, someone has left letters on the line; these also don't change
; its direction, but it can use them to keep track of where it's been. For example:
;
;     |
;     |  +--+
;     A  |  C
; F---|----E|--+
;     |  |  |  D
;     +B-+  +--+
;
;Given this diagram, the packet needs to take the following path:
;
;Starting at the only line touching the top of the diagram, it must go down, pass through A, and continue onward to the first +.
;Travel right, up, and right, passing through B in the process.
;Continue down (collecting C), right, and up (collecting D).
;Finally, go all the way left through E and stopping at F.
;Following the path to the end, the letters it sees on its path are ABCDEF.
;
;The little packet looks up at you, hoping you can help it find the way. What letters will it see
;(in the order it would see them) if it follows the path?
;(The routing diagram is very wide; make sure you view it without line wrapping.)

(defn parse-grid
  [input]
  (->> input
    (str/split-lines)
    (mapv vec)))

(comment
  (def input "     |\n     |  +--+\n     A  |  C\n F---|----E|--+\n     |  |  |  D\n     +B-+  +--+")

  (def input (slurp (io/resource "aoc2017/day19.txt")))
  (def grid (parse-grid input))
  )

(defn cell-value
  [grid pos]
  (let [[i j] pos]
    (-> grid (get i) (get j \space))))

(def letter?
  (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defn v-axis
  [v]
  (case v
    \| :vertical
    \- :horizontal
    nil))

(defn dir-axis
  [[i j]]
  (if (zero? i)
    :horizontal
    :vertical))

(defn turn-right
  [[i j]]
  [j (- i)])

(defn turn-left
  [[i j]]
  [(- j) i])

(defn move-by-dir
  [[ip jp] [id jd]]
  [(+ ip id) (+ jp jd)])

(defn start-pos
  [grid]
  [0
   (let [row (get grid 0)]
     (->> (range (count row))
       (filter (fn [j]
                 (-> row (get j) (= \|))))
       first))])

(defn next-move
  "returns the next direction to move by"
  [grid current-pos current-dir]
  (let [v (cell-value grid current-pos)]
    (case v
      (\| \-)
      current-dir
      \+
      (let [next-dirs
            (->> [(turn-right current-dir)
                  (turn-left current-dir)]
              (filter
                (fn [nd]
                  (not=
                    (cell-value grid (move-by-dir current-pos nd))
                    \space))))
            cnd (count next-dirs)]
        (cond
          (= cnd 0)
          (throw (ex-info "No turn option"
                   {:current-pos current-pos :v v :current-dir current-dir}))
          (> cnd 1)
          (throw (ex-info "Too many turn options"
                   {:current-pos current-pos :v v :current-dir current-dir}))
          :else
          (first next-dirs)))
      \space
      nil
      (cond
        (letter? v) current-dir
        :else (throw (ex-info "Unknown case" {:current-pos current-pos :v v :current-dir current-dir}))))))

(defn walk
  [grid]
  (loop
    [pos (start-pos grid)
     dir [1 0]
     letters []
     n-moves 0]
    (let [v (cell-value grid pos)
          letters (cond-> letters
                    (letter? v) (conj v))]
      (if-some [nd (next-move grid pos dir)]
        (recur
          (move-by-dir pos nd)
          nd
          letters
          (inc n-moves))
        {:pos pos :dir dir
         :letters letters
         :n-moves n-moves}))))

(defn solve1
  [grid]
  (->> (walk grid)
    :letters (apply str)))

(defn print-subgrid
  "for debugging"
  [grid [i0 j0] [i1 j1]]
  (->> (subvec grid i0 i1)
    (mapv #(subvec % j0 j1))
    (map #(apply str %))
    (interpose "\n")
    (apply str)
    println))

(comment

  (print-subgrid
    grid
    [60 52] [66 58])

  (solve1 grid)
  => "DWNBGECOMY"

  )

;--- Part Two ---
;The packet is curious how many steps it needs to go.
;
;For example, using the same routing diagram from the example above...
;
;     |
;     |  +--+
;     A  |  C
; F---|--|-E---+
;     |  |  |  D
;     +B-+  +--+
;
;...the packet would go:
;
;6 steps down (including the first line at the top of the diagram).
;3 steps right.
;4 steps up.
;3 steps right.
;4 steps down.
;3 steps right.
;2 steps up.
;13 steps left (including the F it stops on).
;This would result in a total of 38 steps.
;
;How many steps does the packet need to go?

(defn solve2
  [grid]
  (->> (walk grid) :n-moves))

(comment
  (solve2 grid)
  )
