(ns aoc2017.day11
  (:require [clojure.java.io :as io]))

;--- Day 11: Hex Ed ---
;Crossing the bridge, you've barely reached the other side of the stream when a program comes up to you, clearly in distress. "It's my child process," she says, "he's gotten lost in an infinite grid!"
;
;Fortunately for her, you have plenty of experience with infinite grids.
;
;Unfortunately for you, it's a hex grid.
;
;The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be found to the north, northeast, southeast, south, southwest, and northwest:
;
;  \ n  /
;nw +--+ ne
;  /    \
;-+      +-
;  \    /
;sw +--+ se
;  / s  \
;You have the path the child process took. Starting where he started, you need to determine the fewest number of steps required to reach him. (A "step" means to move from the hex you are in to any adjacent hex.)
;
;For example:
;
;ne,ne,ne is 3 steps away.
;ne,ne,sw,sw is 0 steps away (back where you started).
;ne,ne,s,s is 2 steps away (se,se).
;se,sw,se,sw,sw is 3 steps away (s,s,sw).

(defn parse
  [input]
  (->> (read-string (str "[" input "]"))
    (mapv #(keyword (name %)))))

(defn +v [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defrecord MultiCoords
  [nne nnw nenw])

(defn +mc
  [{u1 :nne u2 :nnw u3 :nenw}
   {v1 :nne v2 :nnw v3 :nenw}]
  (->MultiCoords (+v u1 v1) (+v u2 v2) (+v u3 v3)))

(defn v-norm1
  [[x y]]
  (int (+ (Math/abs (int x)) (Math/abs (int y)))))

(defn mc-norm1
  [{:keys [nne nnw nenw]}]
  (min (v-norm1 nne) (v-norm1 nnw) (v-norm1 nenw)))

(def directions
  {:n (map->MultiCoords {:nne [1 0] :nnw [1 0] :nenw [1 1]})
   :s (map->MultiCoords {:nne [-1 0] :nnw [-1 0] :nenw [-1 -1]})
   :ne (map->MultiCoords {:nne [1 0] :nnw [1 -1] :nenw [1 0]})
   :sw (map->MultiCoords {:nne [-1 0] :nnw [-1 1] :nenw [-1 0]})
   :nw (map->MultiCoords {:nne [1 -1] :nnw [0 1] :nenw [0 1]})
   :se (map->MultiCoords {:nne [-1 1] :nnw [0 -1] :nenw [0 -1]})})

(def origin
  (->MultiCoords [0 0] [0 0] [0 0]))

(defn position
  [parsed]
  (transduce
    (map directions)
    (completing +mc)
    origin parsed))

(defn solve1
  [parsed]
  (mc-norm1 (position parsed)))

(comment
  (solve1 (parse (slurp (io/resource "aoc2017/day11.txt"))))
  )

(defn solve2
  [parsed]
  (->> parsed
    (map directions)
    (reductions +mc origin)
    (map mc-norm1)
    (apply max)))

(comment
  (solve2 (parse (slurp (io/resource "aoc2017/day11.txt"))))
  )




