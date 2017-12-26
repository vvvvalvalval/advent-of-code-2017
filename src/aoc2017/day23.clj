(ns aoc2017.day23
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [aoc2017.day23 Arg]))

(set! *warn-on-reflection* true)

;; ------------------------------------------------------------------------------
;; Parsing

(defn parse
  [input]
  (->> input
    (str/split-lines)
    (map (fn [line]
           (read-string (str "[" line "]"))))))

;; ------------------------------------------------------------------------------
;; Arguments

(defrecord ConstantArg
  [^int v]
  Arg
  (resolveValue [this state]
    v)
  (resolveRegister [this]
    (throw (ex-info "Constant arg can't resolve to register"
             {:arg this}))))

(defrecord RefArg
  [^int regIndex, regName]
  Arg
  (resolveValue [this state]
    (let [^ints state state]
      (aget state regIndex)))
  (resolveRegister [this]
    regIndex))

;; ------------------------------------------------------------------------------
;; Instructions

(defprotocol Instr
  (exec1 [this state] "Modifies the state in place and returns a truthy result if mul was called.
  The state is an array of N+1 ints, cell zero holding the code pointer."))

(defn inc-code-pointer!
  [state]
  (let [^ints state state]
    (aset state 0 (inc (aget state 0)))))

(defrecord Set
  [X Y]
  Instr
  (exec1 [_ state]
    (let [^ints state state]
      (aset state
        (.resolveRegister ^Arg X)
        (.resolveValue ^Arg Y state))
      (inc-code-pointer! state)
      false)))

(defrecord Sub
  [X Y]
  Instr
  (exec1 [_ state]
    (let [^ints state state
          regX (.resolveRegister ^Arg X)
          vY (.resolveValue ^Arg Y state)]
      (aset state
        regX
        (- (aget state regX) vY))
      (inc-code-pointer! state)
      false)))

(defrecord Mul
  [X Y]
  Instr
  (exec1 [_ state]
    (let [^ints state state
          regX (.resolveRegister ^Arg X)
          vY (.resolveValue ^Arg Y state)]
      (aset state
        regX
        (* (aget state regX) vY))
      (inc-code-pointer! state)
      true)))

(defrecord Jnz
  [X Y]
  Instr
  (exec1 [_ state]
    (let [^ints state state
          vX (.resolveValue ^Arg X state)
          vY (.resolveValue ^Arg Y state)]
      (aset state 0
        (+ (aget state 0)
          (if (not (zero? vX))
            vY
            (int 1))))
      false)))

;; ------------------------------------------------------------------------------
;; Compilation

(defn compile-instrs
  [parsed]
  (let [registers
        (->> "abcdefgh"
          (map #(symbol (str %)))
          (map (fn [regIndex regName]
                 [regIndex regName])
            (iterate inc 1))
          (into {}))
        register-names
        (->> registers
          (map (fn [[regIndex regName]]
                 [regName regIndex]))
          (into {}))
        parse-arg (fn [a]
                    (if (symbol? a)
                      (->RefArg (get register-names a) a)
                      (->ConstantArg a)))
        instrs
        (->> parsed
          (map (let [t {'set ->Set
                        'sub ->Sub
                        'mul ->Mul
                        'jnz ->Jnz}]
                 (fn [[op & args]]
                   (apply (get t op) (map parse-arg args)))))
          object-array)]
    {:registers registers
     :register-names register-names
     :instrs instrs}))

;; ------------------------------------------------------------------------------
;; Run

(comment

  (def input
    (slurp (io/resource "aoc2017/day23.txt")))

  (def compiled (->> input parse compile-instrs))

  (seq (:instrs *1))
  )

(defn init-state
  [compiled]
  (int-array (+ 1 (count (:registers compiled))) 0))

(defn exec1-next
  [^objects instrs, ^ints state]
  (let [code-i (aget state 0)
        instr (aget instrs code-i)]
    (exec1 instr state)))

(defn count-mul
  [^objects instrs, ^ints state]
  (let [n-instrs (alength instrs)]
    (loop [n-mul 0]
      (if (-> state (aget 0) (< n-instrs))
        (recur
          (if (exec1-next instrs state)
            (inc n-mul)
            n-mul))
        n-mul))))

(defn solve1
  [compiled]
  (let [{:as compiled :keys [instrs]} compiled
        state (init-state compiled)]
    (count-mul instrs state)))

(comment
  (solve1 compiled)
  => 7071

  (-> compiled :instrs count)
  )

;; ------------------------------------------------------------------------------
;; Part 2

(defn set-register!
  [compiled, ^ints state, regName, value]
  (aset state
    (get (:register-names compiled) regName)
    (int value)))

(defn read-register
  [compiled, ^ints state, regName]
  (aget state
    (int (get (:register-names compiled) regName))))

(defn pretty-state
  [compiled, ^ints state, t]
  [t
   (aget state 0)
   (into {}
     (for [[regName regIndex] (:register-names compiled)]
       [regName (aget state regIndex)]))])

(defn loop11-19
  [^ints state]
  (prn "11-19")
  (let [b (aget state 2)
        d (aget state 4)
        iG (int 7)
        iE (int 5)
        e0 (aget state iE)
        _ (when-not (<= (+ e0 1) b)
            (throw (ex-info "Should have e + 1 <= b to prevent infinite loop"
                     {:b b :e e0})))]
    ;; end in instr 20
    (aset state 0 20)
    ;; exit when g = 0, so g = 0 in the end
    (aset state iG 0)
    ;; e ends with value b, because g = e - b
    (aset state iE b)
    (when (and
            (not (zero? d))
            (-> b (mod d) zero?)
            (let [bd (quot b d)]
              (and (<= e0 bd) (< bd b))))
      (let [iF (int 6)]
        (aset state iF 0)))
    ))

(defn solve2
  [compiled]
  (let [{:as compiled :keys [instrs]} compiled
        ^ints state (init-state compiled)
        n-instrs (alength ^objects instrs)]
    (set-register! compiled state 'a 1)
    (loop [t 0]
      #_(prn (pretty-state compiled state t))
      #_(when (> t 100)
        (throw (ex-info "aaaaargh" {})))
      #_(when (-> t (mod 1000000) (= 1))
        (prn (pretty-state compiled state t)))
      (let [i (aget state 0)]
        (if (< i n-instrs)
          (cond
            (= i (int 11))
            (loop11-19 state)
            :else
            (do
              (exec1-next instrs state)
              (recur (inc t))))
          (read-register compiled state 'h))))))


(comment
  (pretty-state
    compiled
    (let [st (init-state compiled)]
      (set-register! compiled st 'b 42)
      (set-register! compiled st 'h 3)
      st)
    9)

  (->> input
    (str/split-lines)
    (map-indexed (fn [i l]
                   [i l]))
    vec)
  =>
  [[0 "set b 81"]
   [1 "set c b"]
   ;; to 4
   [2 "jnz a 2"]
   ;; to 8
   [3 "jnz 1 5"]
   [4 "mul b 100"]
   [5 "sub b -100000"]
   [6 "set c b"]
   [7 "sub c -17000"]
   ([8 "set f 1"]
     [9 "set d 2"]
     ([10 "set e 2"]
       ;; affects: g, f, e
       ;; params: d, e, b
       ;; summary:
       ;; if (d*e - b = 0) f <- 0
       ;; e <- e + 1
       ;; g <- e - b
       ;; so iterates K = (g0 / b) + 1 times,
       ;; at the end g = 0, e = e0 + K, and if (d divides b and e0 <= b/d < e0 + K) f = 0
       ([11 "set g d"]
         [12 "mul g e"]
         [13 "sub g b"]
         ;; to 16, i.e skip (set f 0)
         ;; e will go through the values e0, e0 + 1, ... until e + 1 - b = 0 i.e e = b - 1
         ;; g will go through the values of {d*e-b, e = e0 ... b - 1}
         ;; so f will be set to zero iff d != 0 d divides b and e0 <= (b / d) < b
         [14 "jnz g 2"]
         [15 "set f 0"]
         [16 "sub e -1"]
         ;; g <- e + 1
         [17 "set g e"]
         ;; g <- g - b = e + 1 - b
         [18 "sub g b"]
         ;; to 11
         [19 "jnz g -8"])
       [20 "sub d -1"]
       [21 "set g d"]
       [22 "sub g b"]
       ;; to 10
       [23 "jnz g -13"])
     ;; to 26
     [24 "jnz f 2"]
     [25 "sub h -1"]
     [26 "set g b"]
     [27 "sub g c"]
     ;; to 30
     [28 "jnz g 2"]
     ;; terminate
     [29 "jnz 1 3"]
     [30 "sub b -17"]
     ;; to 8
     [31 "jnz 1 -23"])
   ]

  (future
    (time (println (solve2 compiled))))
  )



