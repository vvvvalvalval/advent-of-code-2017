(ns aoc2017.day08
  (:require [clojure.string :as str]
            [clojure.edn]))

(comment

  (def input "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10")

  )

(defn parse
  [input]
  (->> input
    (str/split-lines)
    (map #(clojure.edn/read-string (str "[" % "]")))
    (map (fn [[reg1 op arg1 _ reg2 pred arg2]]
           [reg1 op arg1 reg2 pred arg2]))))

(defn init-state
  [parsed]
  (let [regs (->> parsed
               (mapcat (fn [[reg1 _ _ reg2 _ _]]
                         [reg1 reg2]))
               (into #{}))]
    (zipmap regs (repeat 0))))

(comment
  (parse input)
  (init-state *1)
  )

(defn run-instr [state [reg1 op arg1 reg2 pred arg2]]
  (update state reg1
    (fn [v]
      (cond-> v
        ((case pred
           < <
           > >
           <= <=
           >= >=
           == =
           != not=) (get state reg2) arg2)
        ((case op
           inc +
           dec -) arg1)))))


(defn solve1
  [input]
  (let [parsed (parse input)
        state (init-state parsed)
        final-state
        (reduce run-instr
          state parsed)]
    (apply max (vals final-state))))

(comment
  (solve1 input)
  )


(defn solve2
  [input]
  (let [parsed (parse input)
        state (init-state parsed)]
    (->> parsed
      (reductions run-instr state)
      (mapcat vals)
      (apply max))))

(comment
  (solve2 input)
  )
