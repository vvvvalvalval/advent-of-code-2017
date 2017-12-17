(ns aoc2017.utils.graphs)

;; (directed, simple) graphs are represented as adjacency lists, i.e {Vertex [Out Neighbours Vertices]}

(defn directed-dfs
  "Returns a map with keys :p, :f and :l, where
  - :p is the predecessor function for a resulting DFS branching-forest (a map from a vertex to its predecessor)
  - :"
  [dg]
  (loop [coloring (zipmap (keys dg) (repeat false))
         ]))

