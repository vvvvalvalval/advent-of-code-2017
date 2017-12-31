(ns aoc2017.day22
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))
;
;--- Day 22: Sporifica Virus ---
;Diagnostics indicate that the local grid computing cluster has been contaminated with the Sporifica Virus. The grid computing cluster is a seemingly-infinite two-dimensional grid of compute nodes. Each node is either clean or infected by the virus.
;
;To prevent overloading the nodes (which would render them useless to the virus) or detection by system administrators, exactly one virus carrier moves through the network, infecting or cleaning nodes as it moves. The virus carrier is always located on a single node in the network (the current node) and keeps track of the direction it is facing.
;
;To avoid detection, the virus carrier works in bursts; in each burst, it wakes up, does some work, and goes back to sleep. The following steps are all executed in order one time each burst:
;
;If the current node is infected, it turns to its right. Otherwise, it turns to its left.
; (Turning is done in-place; the current node does not change.)
;If the current node is clean, it becomes infected. Otherwise, it becomes cleaned.
; (This is done after the node is considered for the purposes of changing direction.)
;The virus carrier moves forward one node in the direction it is facing.
;Diagnostics have also provided a map of the node infection status (your puzzle input).
; Clean nodes are shown as .; infected nodes are shown as #.
; This map only shows the center of the grid; there are many more nodes beyond those shown,
; but none of them are currently infected.
;
;The virus carrier begins in the middle of the map facing up.
;
;For example, suppose you are given a map like this:
;
;..#
;#..
;...
;Then, the middle of the infinite grid looks like this, with the virus carrier's position marked with [ ]:
;
;. . . . . . . . .
;. . . . . . . . .
;. . . . . . . . .
;. . . . . # . . .
;. . . #[.]. . . .
;. . . . . . . . .
;. . . . . . . . .
;. . . . . . . . .
;The virus carrier is on a clean node, so it turns left, infects the node, and moves left:
;
;. . . . . . . . .
;. . . . . . . . .
;. . . . . . . . .
;. . . . . # . . .
;. . .[#]# . . . .
;. . . . . . . . .
;. . . . . . . . .
;. . . . . . . . .
;The virus carrier is on an infected node, so it turns right, cleans the node, and moves up:
;
;. . . . . . . . .
;. . . . . . . . .
;. . . . . . . . .
;. . .[.]. # . . .
;. . . . # . . . .
;. . . . . . . . .
;. . . . . . . . .
;. . . . . . . . .
;Four times in a row, the virus carrier finds a clean, infects it, turns left, and moves forward, ending in the same place and still facing up:
;
;. . . . . . . . .
;. . . . . . . . .
;. . . . . . . . .
;. . #[#]. # . . .
;. . # # # . . . .
;. . . . . . . . .
;. . . . . . . . .
;. . . . . . . . .
;Now on the same node as before, it sees an infection, which causes it to turn right, clean the node, and move forward:
;
;. . . . . . . . .
;. . . . . . . . .
;. . . . . . . . .
;. . # .[.]# . . .
;. . # # # . . . .
;. . . . . . . . .
;. . . . . . . . .
;. . . . . . . . .
;After the above actions, a total of 7 bursts of activity had taken place. Of them, 5 bursts of activity caused an infection.
;
;After a total of 70, the grid looks like this, with the virus carrier facing up:
;
;. . . . . # # . .
;. . . . # . . # .
;. . . # . . . . #
;. . # . #[.]. . #
;. . # . # . . # .
;. . . . . # # . .
;. . . . . . . . .
;. . . . . . . . .
;By this time, 41 bursts of activity caused an infection (though most of those nodes have since been cleaned).
;
;After a total of 10000 bursts of activity, 5587 bursts will have caused an infection.
;
;Given your actual map, after 10000 bursts of activity, how many bursts cause a node to become infected? (Do not count nodes that begin infected.)

(defn parse-infected
  [input]
  (let [rows
        (->> (str/split-lines input)
          (mapv vec))
        N (count rows)
        N-2 (quot N 2)]
    (into #{}
      cat
      (for [i (range (- N-2) (inc N-2))
            j (range (- N-2) (inc N-2))]
        (when (= (-> rows
                   (get (+ i N-2))
                   (get (+ j N-2)))
                \#)
          [[i j]])))))

(defn turn-right
  [[i j]]
  [j (- i)])

(defn turn-left
  [[i j]]
  [(- j) i])

(defrecord State
  [grid pos dir])

(defn infected?
  [grid pos]
  (contains? grid pos))

(defn clean
  [grid pos]
  (disj grid pos))

(defn infect
  [grid pos]
  (conj grid pos))

(defn move-by-dir
  [[ip jp] [id jd]]
  [(+ ip id) (+ jp jd)])

(defn update-state
  [{:as state :keys [grid pos dir]}]
  (let [inf? (infected? grid pos)]
    [(if inf?
       (let [next-dir (turn-right dir)]
         (->State
           (clean grid pos)
           (move-by-dir pos next-dir)
           next-dir))
       (let [next-dir (turn-left dir)]
         (->State
           (infect grid pos)
           (move-by-dir pos next-dir)
           next-dir)))
     (not inf?)]))

(defn init-state
  [grid]
  (->State grid [0 0] [-1 0]))

(defn solve1
  [grid]
  (loop [t 0
         state (init-state grid)
         caused-inf 0]
    (if (= t 10000)
      caused-inf
      (let [[next-state ci?] (update-state state)]
        (recur
          (inc t)
          next-state
          (if ci? (inc caused-inf) caused-inf))))))

(comment
  (def input "..#\n#..\n...")
  (def input (slurp (io/resource "aoc2017/day22.txt")))

  (def grid (parse-infected input))

  (solve1 grid)
  => 5462
  )










