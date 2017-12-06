(ns aoc2016.day23
  "Building an interpreter of a very basic assembly language that self-modifies its code.
  https://adventofcode.com/2016/day/23

  Performance is critical in the second part of the problem - the difference is between seconds and hours.
  "
  (:require [vvvvalvalval.supdate.api :as supd]
            [clojure.string :as str]
            [aoc2017.utils :as u])
  (:import [aoc2016 Day23 Day23$IInstrArg Day23$Instr Day23$ConstantArg Day23$RefArg Day23$InstrType]))

;; ------------------------------------------------------------------------------
;; Inputs

(comment

  ;; example
  (def input "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a")
  (def a0 0)

  ;; real input
  (def input "cpy a b\ndec b\ncpy a d\ncpy 0 a\ncpy b c\ninc a\ndec c\njnz c -2\ndec d\njnz d -5\ndec b\ncpy b c\ncpy c d\ndec d\ninc c\njnz d -2\ntgl c\ncpy -16 c\njnz 1 c\ncpy 98 c\njnz 91 d\ninc a\ninc d\njnz d -2\ninc c\njnz c -5")

  (def a0 7) ;; first part
  (def a0 12) ;; second part
  )

(defn parse-reg-or-v
  [s]
  (if (re-matches #"\-?\d+" s)
    (u/parse-long s)
    s))

(defn parse
  [input]
  (u/parse-by
    [[:cpy #"cpy.(\S+).(\S+)" parse-reg-or-v parse-reg-or-v]
     [:inc #"inc.(\S+)" identity]
     [:dec #"dec.(\S+)" identity]
     [:jnz #"jnz.(\S+).(\S+)" parse-reg-or-v parse-reg-or-v]
     [:tgl #"tgl.(\S+)" parse-reg-or-v]]
    input))

;; ------------------------------------------------------------------------------
;; First version, idiomatic Clojure (immutable state, multimethods)

(defrecord MachineState
  [code i registers])

(defn init-state
  [code registers]
  (->MachineState
    (vec code)
    0
    registers))

(defn terminated?
  [ms]
  (>= (:i ms) (count (:code ms))))

(defmulti exec-instruction
  (fn [ms [op & args]] op))

(defn exec-instr
  [ms instr]
  (try
    (exec-instruction ms instr)
    (catch Throwable err
      (prn err)
      (update ms :i inc))))

(defn run1
  [code registers]
  (loop [ms (init-state code registers)]
    (if (terminated? ms)
      ms
      (recur (exec-instr ms (get (:code ms) (:i ms)))))))

(defn cpy* [ms x y]
  {:pre [(string? y)]}
  (supd/supdate ms
    {:i inc
     :registers
     (fn [rs]
       (assoc rs
         y
         (if (integer? x)
           x
           (get rs x 0))))}))

(defmethod exec-instruction :cpy
  [ms [_ x y]] (cpy* ms x y))

(defn inc*
  [ms x]
  {:pre [(string? x)]}
  (supd/supdate ms
    {:i inc
     :registers {x inc}}))

(defmethod exec-instruction :inc
  [ms [_ x]] (inc* ms x))

(defn dec*
  [ms x]
  {:pre [(string? x)]}
  (supd/supdate ms
    {:i inc
     :registers {x dec}}))

(defmethod exec-instruction :dec
  [ms [_ x y]] (dec* ms x))

(defn jnz*
  [ms x y]
  (let [rs (:registers ms)]
    (let [jump? (not (zero? (if (integer? x)
                              x
                              (get rs x 0))))]
      (supd/supdate ms
        {:i #(+ % (if jump?
                    (if (integer? y)
                      y
                      (get rs y 0))
                    1))}))))

(defmethod exec-instruction :jnz
  [ms [_ x y]] (jnz* ms x y))

(defn toggle-instr
  [[op & args]]
  (into
    [(case op
       :inc :dec
       (:tgl :dec) :inc
       :jnz :cpy
       :cpy :jnz)]
    args))

(defn tgl*
  [{:as ms :keys [registers i code]} x]
  (let [rs (:registers ms)
        offset (if (integer? x)
                 x
                 (get rs x 0))]
    (supd/supdate ms
      {:i inc
       :code {(+ i offset) toggle-instr}})))

(defmethod exec-instruction :tgl
  [ms [_ x]] (tgl* ms x))


(comment
  (future
    (let [code (parse input)
          registers (assoc (zipmap ["a" "b" "c" "d"] (repeat 0))
                      "a" a0)]
      (time
        ;; about 600ms for a0 = 7
        (dotimes [_ 10]
          (run1 code registers)))))
  )

;; ------------------------------------------------------------------------------
;; Second version - mutable state made of arrays and objects + enums
;; runs about 100 times faster than the previous one

(set! *warn-on-reflection* true)

(defn to-java-arg
  ^Day23$IInstrArg [arg1]
  (cond
    (string? arg1)
    (Day23$RefArg.
      (case arg1
        "a" 0
        "b" 1
        "c" 2
        "d" 3))

    (integer? arg1)
    (Day23$ConstantArg. arg1)

    (nil? arg1)
    nil))

(defn from-java-reg
  [reg-index]
  (get ["a" "b" "c" "d"] reg-index))

(defn from-java-arg
  [^Day23$IInstrArg jarg]
  (cond
    (nil? jarg)
    nil

    (instance? Day23$RefArg jarg)
    (from-java-reg (.-reg ^Day23$RefArg jarg))

    (instance? Day23$ConstantArg jarg)
    (.-v ^Day23$ConstantArg jarg)))

(defn to-java-instr
  ^Day23$Instr [[op arg1 arg2]]
  (let [arg1 (to-java-arg arg1)
        arg2 (to-java-arg arg2)
        it (case op
             :cpy Day23$InstrType/CPY
             :inc Day23$InstrType/INC
             :dec Day23$InstrType/DEC
             :jnz Day23$InstrType/JNZ
             :tgl Day23$InstrType/TGL)]
    (Day23$Instr. it arg1 arg2)))

(defn from-java-instr
  [^Day23$Instr instr]
  (into
    [(get {Day23$InstrType/CPY :cpy
           Day23$InstrType/INC :inc
           Day23$InstrType/DEC :dec
           Day23$InstrType/JNZ :jnz
           Day23$InstrType/TGL :tgl}
       (.instrType instr))
     (from-java-arg (.-arg1 instr))]
    (when-let [a2 (from-java-arg (.-arg2 instr))]
      [a2])))

(defn ms-from-java
  [i, ^ints registers-arr, ^objects code-arr]
  (->MachineState
    (into []
      (map from-java-instr)
      code-arr)
    i
    (into {}
      (map-indexed
        (fn [j v]
          [(from-java-reg j)
           v]))
      registers-arr)))

(defn run2
  [code registers]
  (let [^objects code-arr
        (into-array Day23$Instr
          (map to-java-instr code))
        ^ints registers-arr
        (into-array Integer/TYPE
          (->> ["a" "b" "c" "d"]
            (map (fn [l]
                   (get registers l 0)))))
        n (alength code-arr)]
    (loop [i (int 0)]
      (if (< i n)
        (let [instr (aget code-arr i)
              next-i (Day23/execInstr i registers-arr code-arr instr)]
          (recur next-i))
        (ms-from-java i registers-arr code-arr)))))

(comment
  (future
    (let [code (parse input)
          registers (assoc (zipmap ["a" "b" "c" "d"] (repeat 0))
                      "a" a0)]
      ;; about 6ms for a0 = 7
      (time
        (dotimes [_ 1]
          (run2 code registers)))))
  )