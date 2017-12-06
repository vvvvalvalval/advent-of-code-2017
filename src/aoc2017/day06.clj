(ns aoc2017.day06)

(def input
  [0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11])

(defn solve
  [input part]
  (let [n (count input)]
    (loop [t 0
           seen {input 0}
           banks (vec input)]
      (let [max-i
            ;; max-key won't work here: "ties won by the lowest-numbered memory bank"
            (reduce
              (fn [max-i i]
                (if (>
                      (nth banks i)
                      (nth banks max-i))
                  i max-i))
              0 (range 1 n))
            n-max (get banks max-i)
            q (quot n-max n)
            r (rem n-max n)
            next-banks
            (loop [i 0
                   tbs (assoc! (transient banks) max-i 0)]
              (if (< i n)
                (let [j (mod (+ max-i 1 i) n)]
                  (recur
                    (inc i)
                    (assoc! tbs j
                      (+ (nth tbs j) q (if (< i r) 1 0)))))
                (persistent! tbs)))
            next-t (inc t)]
        (if-let [previous-t (get seen next-banks)]
          (case part
            1 next-t
            2 (- next-t previous-t))
          (recur next-t (assoc seen next-banks next-t) next-banks))))))

(solve input 2)
