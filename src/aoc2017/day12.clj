(ns aoc2017.day12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;--- Day 12: Digital Plumber ---
;Walking along the memory banks of the stream, you find a small village that is experiencing a little confusion: some programs can't communicate with each other.
;
;Programs in this village communicate using a fixed system of pipes. Messages are passed between programs using these pipes, but most programs aren't connected to each other directly. Instead, programs pass messages between each other until the message reaches the intended recipient.
;
;For some reason, though, some of these messages aren't ever reaching their intended recipient, and the programs suspect that some pipes are missing. They would like you to investigate.
;
;You walk through the village and record the ID of each program and the IDs with which it can communicate directly (your puzzle input). Each program has one or more programs with which it can communicate, and these pipes are bidirectional; if 8 says it can communicate with 11, then 11 will say it can communicate with 8.
;
;You need to figure out how many programs are in the group that contains program ID 0.
;
;For example, suppose you go door-to-door like a travelling salesman and record the following list:
;
;0 <-> 2
;1 <-> 1
;2 <-> 0, 3, 4
;3 <-> 2, 4
;4 <-> 2, 3, 6
;5 <-> 6
;6 <-> 4, 5
;In this example, the following programs are in the group that contains program ID 0:
;
;Program 0 by definition.
;Program 2, directly connected to program 0.
;Program 3 via program 2.
;Program 4 via program 2.
;Program 5 via programs 6, then 4, then 2.
;Program 6 via programs 4, then 2.
;Therefore, a total of 6 programs are in this group; all but program 1, which has a pipe that connects it to itself.
;
;How many programs are in the group that contains program ID 0?
;

(defn parse [input]
  (->> input
    (str/split-lines)
    (map (fn [l] (read-string (str "[" l "]"))))
    (mapv (fn [[v _ & neighbours]]
            [v neighbours]))))

(comment
  (def parsed (parse (slurp (io/resource "aoc2017/day12.txt"))))
  )

(defn adjacency-list
  [parsed]
  (->> parsed
    (mapcat (fn [[v neighbours]]
              (for [n neighbours]
                [v n])))
    (reduce
      (fn [al [v n]]
        (if (not= v n)
          (-> al
            (update v #(-> % (or #{}) (conj n)))
            (update n #(-> % (or #{}) (conj v))))
          al))
      {})))

(comment
  (adjacency-list parsed)
  )

(defn walk
  [al state v]
  (if (contains? state v)
    state
    (let [neighbours (get al v #{})]
      (as-> state state
        (assoc state v ::visiting)
        (reduce #(walk al %1 %2) state neighbours)
        (assoc state
          v
          (->> neighbours
            (map state)
            (remove #{::visiting})
            (reduce into (conj neighbours v))))))))

(defn find-reachable
  [al v0]
  (get (walk al {} v0) v0))

(defn solve1
  [parsed]
  (count (find-reachable (adjacency-list parsed) 0)))

(comment
  (solve1 parsed)
  => 288)
;
;--- Part Two ---
;There are more programs than just the ones in the group containing program ID 0. The rest of them have no way of reaching that group, and still might have no way of reaching each other.
;
;A group is a collection of programs that can all communicate via pipes either directly or indirectly. The programs you identified just a moment ago are all part of the same group. Now, they would like you to determine the total number of groups.
;
;In the example above, there were 2 groups: one consisting of programs 0,2,3,4,5,6, and the other consisting solely of program 1.
;
;How many groups are there in total?
;

(defn solve2
  [parsed]
  (let [al (adjacency-list parsed)]
    (->> parsed
      (map (fn [[v _]]
             [v (apply min (get (walk al {} v) v))]))
      (map second)
      distinct)))
(comment
  (solve2 parsed)
  => 211)



