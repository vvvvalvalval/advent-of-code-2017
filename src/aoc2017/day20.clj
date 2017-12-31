(ns aoc2017.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

;--- Day 20: Particle Swarm ---
;Suddenly, the GPU contacts you, asking for help. Someone has asked it to simulate too many particles, and it won't be able to finish them all in time to render the next frame at this rate.
;
;It transmits to you a buffer (your puzzle input) listing each particle in order (starting with particle 0, then particle 1, particle 2, and so on). For each particle, it provides the X, Y, and Z coordinates for the particle's position (p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.
;
;Each tick, all particles are updated simultaneously. A particle's properties are updated in the following order:
;
;Increase the X velocity by the X acceleration.
;Increase the Y velocity by the Y acceleration.
;Increase the Z velocity by the Z acceleration.
;Increase the X position by the X velocity.
;Increase the Y position by the Y velocity.
;Increase the Z position by the Z velocity.
;Because of seemingly tenuous rationale involving z-buffering, the GPU would like to know which particle will stay closest to position <0,0,0> in the long term. Measure this using the Manhattan distance, which in this situation is simply the sum of the absolute values of a particle's X, Y, and Z position.
;
;For example, suppose you are only given two particles, both of which stay entirely on the X-axis (for simplicity). Drawing the current states of particles 0 and 1 (in that order) with an adjacent a number line and diagram of current X positions (marked in parenthesis), the following would take place:
;
;p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
;p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)
;
;p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
;p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)
;
;p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
;p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)
;
;p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
;p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)
;At this point, particle 1 will never be closer to <0,0,0> than particle 0, and so, in the long run, particle 0 will stay closest.
;
;Which particle will stay closest to position <0,0,0> in the long term?

(defn parse-particles
  [input]
  (->> input
    str/split-lines
    (map (fn [l]
           (read-string
             (str "{"
               (-> l
                 (str/replace #"(\w)=" ":$1 ")
                 (str/replace \< \[)
                 (str/replace \> \]))
               "}"))))
    (mapv
      (let [rw-paths (for [[c i] (map vector [:x :y :z] (range))
                           [k1 k2] [[:p :p0]
                                    [:v :v0]
                                    [:a :a0]]]
                       [[k1 i] [c k2]])]
        (fn [m]
          (reduce (fn [ret [rp wp]]
                    (assoc-in ret wp (get-in m rp)))
            {} rw-paths))))))

(comment
  (def input (slurp (io/resource "aoc2017/day20.txt")))

  (def particles (parse-particles input))
  )

;; a little back-of-the-envelope math gives us an explicit formula
;; for the position of any particle at any time
(defn coord-at
  [{:as coord :keys [a0 v0 p0]} t]
  (+
    p0
    (* v0 t)
    (-> a0 (* t (inc t)) (/ 2))))

(defn pos-at
  [particle t]
  (reduce-kv
    (fn [ret k coord]
      (assoc ret k (coord-at coord t)))
    {} particle))

(defn abs
  [v]
  (if (neg? v) (- v) v))

(defn dist-to-origin
  [pos]
  (->> pos vals (map abs) (apply +)))

(comment
  (dist-to-origin {:x 1 :y -2 :z 0}))

(defn solve1
  [particles]
  (apply min-key
    (fn [i]
      (-> particles (get i) (pos-at 1e6) dist-to-origin))
    (range (count particles))))

(comment
  (solve1 particles)
  )

;--- Part Two ---
;To simplify the problem further, the GPU would like to remove any particles that collide. Particles collide if their positions ever exactly match. Because particles are updated simultaneously, more than two particles can collide at the same time and place. Once particles collide, they are removed and cannot collide with anything else after that tick.
;
;For example:
;
;p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
;p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
;p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
;p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>
;
;p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>
;p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
;p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)
;p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>
;
;p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>
;p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
;p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)
;p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>
;
;------destroyed by collision------
;------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
;------destroyed by collision------                      (3)
;p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>
;In this example, particles 0, 1, and 2 are simultaneously destroyed at the time and place marked X. On the next tick, particle 3 passes through unharmed.
;
;How many particles are left after all collisions are resolved?
;

;;;; a faster representation of particles, as we'll now need to compute many iterations.

(defrecord FastPos
  [^long x ^long y ^long z])

(defprotocol Particle
  (pos-at-t [this t]))

(defn coord-at*
  ^long [^long p0 ^long v0 ^long a0 ^long t]
  (+
    p0
    (* v0 t)
    (-> a0 (* t (inc t)) (/ 2) long)))

;; some runtime compilation to get excellent performance
(eval
  (let [t-sym (gensym "t")]
    `(defrecord ~'FastParticle
       [~@(for [c '[x y z]
                k '[p0 v0 a0]]
            (-> (symbol (str c k))
              ;; primitive type hints for the fields
              (vary-meta merge {:tag `long})))]
       Particle
       (~'pos-at-t [this# ~t-sym]
         (let [~t-sym (long ~t-sym)]
           (FastPos.
             ~@(for [c '[x y z]]
                 `(coord-at*
                    ~(symbol (str c 'p0))
                    ~(symbol (str c 'v0))
                    ~(symbol (str c 'a0))
                    ~t-sym))))))))

(defn compile-particle
  [p]
  (FastParticle.
    (-> p :x :p0)
    (-> p :x :v0)
    (-> p :x :a0)
    (-> p :y :p0)
    (-> p :y :v0)
    (-> p :y :a0)
    (-> p :z :p0)
    (-> p :z :v0)
    (-> p :z :a0)))

(defn remove-collisions
  [ps ^long t]
  (->> ps
    (group-by #(pos-at-t % t))
    vals
    (into []
      (comp
        (remove #(-> % count (> 1)))
        (map first)))))

(defn solve2
  [particles]
  (loop [ps (mapv compile-particle particles)
         t 0]
    (if (> t 1000)
      (count ps)
      (let [next-ps (remove-collisions ps t)]
        #_(when (< (count next-ps) (count ps))
          (prn "Collision(s) at t" t))
        (recur
          next-ps
          (inc t))))))

(comment

  (dotimes [_ 5]
    (time
      (dotimes [_ 5]
        (solve2 particles)
        )))
  => 502
  )

