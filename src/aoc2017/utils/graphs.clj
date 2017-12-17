(ns aoc2017.utils.graphs
  (:require [clojure.set]))

;; (directed, simple) graphs are represented as adjacency lists, i.e {Vertex [Out-Neighbours-Vertices]}

;; ------------------------------------------------------------------------------
;; Graph creation

(defn digraph-from-pairs
  [pairs]
  (persistent!
    (reduce
      (fn [dg [u v]]
        (cond-> (assoc! dg u
                  (conj (get dg u #{}) v))
          (not (get dg v))
          (assoc! v #{})))
      (transient {}) pairs)))

(comment
  (digraph-from-pairs
    [[0 1]
     [1 2]
     [2 0]
     [2 3]])
  => {0 #{1}, 1 #{2}, 2 #{0 3}, 3 #{}}
  )

;; ------------------------------------------------------------------------------
;; Algorithms

(defn components
  "Given a (presumably undirected) graph, computes the components of that graph."
  [g]
  (letfn [(visit [tc v]
            (if (contains? tc v)
              tc
              (let [neighbours (get g v #{})]
                (reduce visit (conj! tc v) neighbours))))
          (component-of [v]
            (persistent! (visit (transient #{}) v)))]
    (loop [remaining (set (keys g))
           ret []]
      (if (empty? remaining)
        ret
        (let [c (component-of (first remaining))]
          (recur
            (persistent! (reduce disj! (transient remaining) c))
            (conj ret c)))))))

(comment
  (components
    {0 []
     1 [2]
     2 [3]
     3 [1]
     4 [5 6]
     5 []
     6 []
     7 []
     8 [8]})
  => [#{0} #{7} #{1 3 2} #{4 6 5} #{8}]
  )


