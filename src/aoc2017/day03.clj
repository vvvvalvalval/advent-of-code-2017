(ns aoc2017.day03)

(def directions
  [[0 1] [1 0] [0 -1] [-1 0]])

(defn moves
  []
  (let [amplitudes (->> (range)
                     (drop 1)
                     (mapcat (fn [x] [x x])))
        dirs (cycle directions)]
    (map (fn [amp dir]
           {:amplitude amp :direction dir})
      amplitudes dirs)))

(defn moved-pos
  [start amp dir]
  (let [[x y] start
        [u v] dir]
    [(+ x (* amp u))
     (+ y (* amp v))]))

(defn turning-points
  []
  (->> {:position [0 0]
        :idx 1
        :next-moves (moves)}
    (iterate (fn [p]
               (let [[mv & nmvs] (:next-moves p)]
                 {:position (moved-pos (:position p) (:amplitude mv) (:direction mv))
                  :idx (+ (:idx p) (:amplitude mv))
                  :next-moves nmvs})))
    (map (fn [{:as p, :keys [next-moves]}]
           (-> p (dissoc :next-moves) (assoc :next-move (first next-moves)))))))

(defn points []
  (->> (turning-points)
    (mapcat (fn [{:keys [position idx next-move]}]
              (let [{:keys [amplitude direction]} next-move]
                (->> (range amplitude)
                  (map (fn [amp]
                         {:position (moved-pos position amp direction)
                          :idx (+ idx amp)}))))))))

(defn l1-dist
  [pos1 pos2]
  (let [[x1 y1] pos1
        [x2 y2] pos2]
    (long
      (+
        (Math/abs (int (- x1 x2)))
        (Math/abs (int (- y1 y2)))))))

(l1-dist [0 1] [3 2])

(defn solve1
  [n]
  (let [{:keys [position]} (nth (points) (dec n))]
    (l1-dist position [0 0])))

(defn neighbours-sum
  [grid-state pos]
  (let [[x y] pos]
    (apply +
      (for [u [1 0 -1]
            v [1 0 -1]
            :when (not (= 0 u v))]
        (get grid-state [(+ x u) (+ y v)] 0)))))

(defn solve2
  [n]
  (loop [gs {[0 0] 1}
         ps (drop 1 (points))]
    (let [{:keys [position]} (first ps)
          written (neighbours-sum gs position)]
      (if (> written n)
        written
        (recur
          (assoc gs position written)
          (next ps))))))



