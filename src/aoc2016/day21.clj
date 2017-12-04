(ns aoc2016.day21
  (:require [aoc2017.utils :as u]
            [clojure.string :as str]))

(defn swap-pos
  [v x y]
  (assoc v
    x (get v y)
    y (get v x)))


(defn swap-letters
  [v x y]
  (let [sub {x y
             y x}]
    (mapv #(get sub % %) v)))

(defn rotate
  [v lor steps]
  (let [offset (* steps (case lor
                          :left 1
                          :right -1))]
    (try
      (->> v cycle (drop (mod offset (count v)))
        (take (count v)) vec)
      (catch Throwable err
        (sc.api/spy)
        (throw err)))))

(defn move-by-pos
  [v x]
  (let [i (->> (range (count v))
            (filter (fn [i]
                      (= (get v i) x)))
            first)]
    (rotate v :right
      (+ 1 i (if (>= i 4) 1 0)))))

(defn undo-move-by-pos
  [v x]
  (or
    (first
      (filter some?
        (for [i (range (count v))]
          (let [v1 (rotate v :left i)]
            (when (= v (move-by-pos v1 x))
              v1)))))
    (throw (ex-info "Can't undo move by pos"
             {:v v :x x}))))

(defn reverse-pos
  [v x y]
  (vec
    (concat
      (take x v)
      (->> v (take (inc y)) (drop x) reverse)
      (drop (inc y) v))))

(defn mov-pos
  [v x y]
  (let [v1 (vec
             (concat
               (take x v)
               (->> v (drop x) rest)))]
    (vec
      (concat
        (take y v1)
        [(get v x)]
        (drop y v1)))))

(defn exec [cmd v]
  (let [[op & args] cmd]
    (apply
      (case op
        :swap-pos swap-pos
        :swap-letters swap-letters
        :rotate rotate
        :move-by-pos move-by-pos
        :reverse-pos reverse-pos
        :mov-pos mov-pos
        :undo-move-by-pos undo-move-by-pos)
      v
      args)))

(defn parse-line
  [i]
  (if-let [[_ x y]
           (re-matches
             #"swap.position.(\d+).with.position.(\d+)"
             i)]
    [:swap-pos (u/parse-long x) (u/parse-long y)]
    (if-let [[_ x y]
             (re-matches
               #"swap.letter.(.).with.letter.(.)"
               i)]
      [:swap-letters x y]
      (if-let [[_ x y]
               (re-matches
                 #"reverse.positions.(\d+).through.(\d+)"
                 i)]
        [:reverse-pos (u/parse-long x) (u/parse-long y)]
        (if-let [[_ x y]
                 (re-matches
                   #"move.position.(\d+).to.position.(\d+)"
                   i)]
          [:mov-pos (u/parse-long x) (u/parse-long y)]
          (if-let [[_ x]
                   (re-matches
                     #"rotate.based.on.position.of.letter.(.)"
                     i)]
            [:move-by-pos x]
            (if-let [[_ x y]
                     (re-matches
                       #"rotate.(right|left).(\d+).step.?"
                       i)]
              [:rotate (case x "right" :right "left" :left) (u/parse-long y)]
              (throw (ex-info "aaaargs" {:i i})))))))))

(defn reverse-cmd
  [[op & args]]
  (case op
    :swap-pos (into [op] args)
    :swap-letters (into [op] args)
    :rotate (let [[dir steps] args]
              [:rotate (case dir :left :right :right :left) steps])
    :mov-pos (into [:mov-pos] (reverse args))
    :reverse-pos (into [op] args)
    :move-by-pos (into [:undo-move-by-pos] args)
    ))

(defn parse-input
  [i]
  (->> i str/split-lines (mapv parse-line)))

(defn solve1
  [commands pwd]
  (apply str
    (reduce (fn [v cmd] (exec cmd v))
      (mapv str pwd) commands)))

(apply str
  (exec
    [:swap-pos 4 0]
    (mapv str "abcde")))

(defn solve2
  [commands pwd]
  (apply str
    (reduce (fn [v cmd] (exec cmd v))
      (mapv str pwd)
      (->> commands reverse (map reverse-cmd)))
    ))

