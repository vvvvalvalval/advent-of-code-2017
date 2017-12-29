(ns aoc2017.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc2017.utils :as u]))

;--- Day 24: Electromagnetic Moat ---
;The CPU itself is a large, black building surrounded by a bottomless pit. Enormous metal tubes extend outward from the side of the building at regular intervals and descend down into the void. There's no way to cross, but you need to get inside.
;
;No way, of course, other than building a bridge out of the magnetic components strewn about nearby.
;
;Each component has two ports, one on each end. The ports come in all different types, and only matching types can be connected. You take an inventory of the components by their port types (your puzzle input). Each port is identified by the number of pins it uses; more pins mean a stronger connection for your bridge. A 3/7 component, for example, has a type-3 port on one side, and a type-7 port on the other.
;
;Your side of the pit is metallic; a perfect surface to connect a magnetic, zero-pin port. Because of this, the first port you use must be of type 0. It doesn't matter what type of port you end with; your goal is just to make the bridge as strong as possible.
;
;The strength of a bridge is the sum of the port types in each component. For example, if your bridge is made of components 0/3, 3/7, and 7/4, your bridge has a strength of 0+3 + 3+7 + 7+4 = 24.
;
;For example, suppose you had the following components:
;
;0/2
;2/2
;2/3
;3/4
;3/5
;0/1
;10/1
;9/10
;With them, you could make the following valid bridges:
;
;0/1
;0/1--10/1
;0/1--10/1--9/10
;0/2
;0/2--2/3
;0/2--2/3--3/4
;0/2--2/3--3/5
;0/2--2/2
;0/2--2/2--2/3
;0/2--2/2--2/3--3/4
;0/2--2/2--2/3--3/5
;(Note how, as shown by 10/1, order of ports within a component doesn't matter.
; However, you may only use each port on a component once.)
;
;Of these bridges, the strongest one is 0/1--10/1--9/10;
; it has a strength of 0+1 + 1+10 + 10+9 = 31.
;
;What is the strength of the strongest bridge you can make with the components you have available?


(def empty-cpnts {})

(defn add-cpnt
  [cpnts cpnt]
  (reduce
    (fn [cpnts port]
      (update cpnts port #(-> % (or #{}) (conj cpnt))))
    cpnts
    cpnt))

(defn having-port
  [cpnts port]
  (get cpnts port))

(defn remove-cpnt
  [cpnts cpnt]
  (reduce
    (fn [cpnts port]
      (update cpnts port #(disj % cpnt)))
    cpnts
    cpnt))

(defn parse-cpnts
  [input]
  (->> input
    (str/split-lines)
    (map (fn [l]
           (->> (str/split l #"/")
             (map u/parse-long)
             set)))
    (reduce add-cpnt empty-cpnts)))

(comment
  (def input
    "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10")
  (def input (slurp (io/resource "aoc2017/day24.txt")))

  (def cpnts (parse-cpnts input))
  )

(defn other-port
  [cpnt from-port]
  (if (-> cpnt count (= 1))
    from-port
    (-> cpnt (disj from-port) first)))


(defn max-brigde-weight
  [cpnts from-port]
  (->>
    (having-port cpnts from-port)
    (transduce
      (map (fn [cpnt]
             (let [to-port (other-port cpnt from-port)]
               (+
                 from-port to-port
                 (max-brigde-weight
                   (remove-cpnt cpnts cpnt)
                   to-port)))))
      max 0)))

(defn solve1
  [cpnts]
  (max-brigde-weight cpnts 0))

(comment
  (solve1 cpnts)
  => 1868
  )

;--- Part Two ---
;The bridge you've built isn't long enough; you can't jump the rest of the way.
;
;In the example above, there are two longest bridges:
;
;0/2--2/2--2/3--3/4
;0/2--2/2--2/3--3/5
;Of them, the one which uses the 3/5 component is stronger; its strength is 0+2 + 2+2 + 2+3 + 3+5 = 19.
;
;What is the strength of the longest bridge you can make? If you can make multiple bridges of the longest length, pick the strongest one.
;
;Although it hasn't changed, you can still get your puzzle input.
;



