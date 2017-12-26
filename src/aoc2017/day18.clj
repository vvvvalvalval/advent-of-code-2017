(ns aoc2017.day18
  (:require [clojure.string :as str])
  (:import [aoc2017.day18 Arg]))

;; the state is represented as an ints array,
;; the first value of which is the index of the current instruction,
;; the second value of which is the last played frequency,
;; the rest holding the register values.

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
  (exec [this state] "Modifies the state in place
  and returns the last played frequency if recovered, nil otherwise"))

(defn inc-code-pointer!
  [state]
  (let [^ints state state]
    (aset state 0 (inc (aget state 0)))))

(defrecord Snd
  [X]
  Instr
  (exec [_ state]
    (let [^ints state state]
      (aset state 1 (.resolveValue ^Arg X state))
      (inc-code-pointer! state)
      nil)))

(defrecord Set
  [X Y]
  Instr
  (exec [_ state]
    (let [^ints state state]
      (aset state
        (.resolveRegister ^Arg X)
        (.resolveValue ^Arg Y state))
      (inc-code-pointer! state)
      nil)))

(defrecord Add
  [X Y]
  Instr
  (exec [_ state]
    (let [^ints state state
          regX (.resolveRegister ^Arg X)
          vY (.resolveValue ^Arg Y state)]
      (aset state
        regX
        (+ (aget state regX) vY))
      (inc-code-pointer! state)
      nil)))

(defrecord Mul
  [X Y]
  Instr
  (exec [_ state]
    (let [^ints state state
          regX (.resolveRegister ^Arg X)
          vY (.resolveValue ^Arg Y state)]
      (aset state
        regX
        (* (aget state regX) vY))
      (inc-code-pointer! state)
      nil)))

(defrecord Mod
  [X Y]
  Instr
  (exec [_ state]
    (let [^ints state state
          regX (.resolveRegister ^Arg X)
          vY (.resolveValue ^Arg Y state)]
      (aset state
        regX
        (int (mod (aget state regX) vY)))
      (inc-code-pointer! state)
      nil)))

(defrecord Rcv
  [X]
  Instr
  (exec [_ state]
    (let [^ints state state
          vX (.resolveValue ^Arg X state)]
      (inc-code-pointer! state)
      (if (zero? vX)
        nil
        (aget state 1)))))

(defrecord Jgz
  [X Y]
  Instr
  (exec [_ state]
    (let [^ints state state
          vX (.resolveValue ^Arg X state)
          vY (.resolveValue ^Arg Y state)]
      (aset state 0
        (+ (aget state 0)
          (if (> vX 0)
            vY
            (int 1))))
      nil)))

;; ------------------------------------------------------------------------------
;; Compilation

(defn compile-instrs
  [parsed]
  (let [registers
        (->> parsed
          (mapcat rest)
          (filter symbol?)
          set sort
          (map (fn [regIndex regName]
                 [regIndex regName])
            (iterate inc 2))
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
          (map (let [t {'snd ->Snd
                        'set ->Set
                        'add ->Add
                        'mul ->Mul
                        'mod ->Mod
                        'rcv ->Rcv
                        'jgz ->Jgz}]
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
    "set a 1
    add a 2
    mul a a
    mod a 5
    snd a
    set a 0
    rcv a
    jgz a -1
    set a 1
    jgz a -2")

  (def input
    "set i 31
    set a 1
    mul p 17
    jgz p p
    mul a 2
    add i -1
    jgz i -2
    add a -1
    set i 127
    set p 826
    mul p 8505
    mod p a
    mul p 129749
    add p 12345
    mod p a
    set b p
    mod b 10000
    snd b
    add i -1
    jgz i -9
    jgz a 3
    rcv b
    jgz b -1
    set f 0
    set i 126
    rcv a
    rcv b
    set p a
    mul p -1
    add p b
    jgz p 4
    snd a
    set a b
    jgz 1 3
    snd b
    set f 1
    add i -1
    jgz i -11
    snd a
    jgz f -16
    jgz a -19")

  (def compiled (->> input parse compile-instrs))

  )

(defn init-state
  [compiled]
  (int-array (+ 2 (count (:registers compiled))) 0))

(defn exec-next
  [^objects instrs, ^ints state]
  (let [code-i (aget state 0)
        instr (aget instrs code-i)]
    (exec instr state)))

(defn find-recovered
  [instrs, state]
  (loop [t 0]
    #_(when (< 1e7 t (+ 1e7 100))
      (let [code-i (aget state 0)
            instr (aget instrs code-i)]
        (vec state)
        (sc.api/spy [code-i instr])))
    (when (> t (+ 1e7 10))
      (throw (ex-info "to far" {})))
    (if-some [recovered (exec-next instrs state)]
      (do
        (println t)
        recovered)
      (recur (inc t)))))

(defn solve1
  [compiled]
  (let [{:as compiled :keys [instrs]} compiled
        state (init-state compiled)]
    (find-recovered instrs state)))

(comment
  (solve1 compiled)
  => 7071
  )