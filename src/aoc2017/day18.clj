(ns aoc2017.day18
  (:require [clojure.string :as str]
            [vvvvalvalval.supdate.api :as supd])
  (:import [aoc2017.day18 Arg]
           (java.util.concurrent LinkedBlockingDeque BlockingQueue BlockingDeque)))

;--- Day 18: Duet ---
;You discover a tablet containing some strange assembly code labeled simply "Duet". Rather than bother the sound card with it, you decide to run the code yourself. Unfortunately, you don't see any documentation, so you're left to figure out what the instructions mean on your own.
;
;It seems like the assembly is meant to operate on a set of registers that are each named with a single letter and that can each hold a single integer. You suppose each register should start with a value of 0.
;
;There aren't that many instructions, so it shouldn't be hard to figure out what they do. Here's what you determine:
;
;snd X plays a sound with a frequency equal to the value of X.
;set X Y sets register X to the value of Y.
;add X Y increases register X by the value of Y.
;mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
;mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
;rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
;jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
;Many of the instructions can take either a register (a single letter) or a number. The value of a register is the integer it contains; the value of a number is that number.
;
;After each jump instruction, the program continues with the instruction to which the jump jumped. After any other instruction, the program continues with the next instruction. Continuing (or jumping) off either end of the program terminates it.
;
;For example:
;
;set a 1
;add a 2
;mul a a
;mod a 5
;snd a
;set a 0
;rcv a
;jgz a -1
;set a 1
;jgz a -2
;The first four instructions set a to 1, add 2 to it, square it, and then set it to itself modulo 5, resulting in a value of 4.
;Then, a sound with frequency 4 (the value of a) is played.
;After that, a is set to 0, causing the subsequent rcv and jgz instructions to both be skipped (rcv because a is 0, and jgz because a is not greater than 0).
;Finally, a is set to 1, causing the next jgz instruction to activate, jumping back two instructions to another jump, which jumps again to the rcv, which ultimately triggers the recover operation.
;At the time the recover operation is executed, the frequency of the last sound played is 4.
;
;What is the value of the recovered frequency (the value of the most recently played sound) the first time a rcv instruction is executed with a non-zero value?


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
  [^long v]
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
    (let [^longs state state]
      (aget state regIndex)))
  (resolveRegister [this]
    regIndex))

;; ------------------------------------------------------------------------------
;; Instructions

(defprotocol Instr
  (exec [this state] "Modifies the state in place
  and returns the last played frequency if recovered, nil otherwise")
  (exec2 [this state snd! rcv!]))

(defn inc-code-pointer!
  [state]
  (let [^longs state state]
    (aset state 0 (inc (aget state 0)))))

(defrecord Snd
  [X]
  Instr
  (exec [_ state]
    (let [^longs state state]
      (aset state 1 (.resolveValue ^Arg X state))
      (inc-code-pointer! state)
      nil))
  (exec2 [_ state snd! _]
    (let [^longs state state]
      (snd! (.resolveValue ^Arg X state))
      (inc-code-pointer! state)
      nil)))

(defrecord Set
  [X Y]
  Instr
  (exec [_ state]
    (let [^longs state state]
      (aset state
        (.resolveRegister ^Arg X)
        (.resolveValue ^Arg Y state))
      (inc-code-pointer! state)
      nil))
  (exec2 [this state _ _]
    (exec this state)))

(defrecord Add
  [X Y]
  Instr
  (exec [_ state]
    (let [^longs state state
          regX (.resolveRegister ^Arg X)
          vY (.resolveValue ^Arg Y state)]
      (aset state
        regX
        (+ (aget state regX) vY))
      (inc-code-pointer! state)
      nil))
  (exec2 [this state _ _]
    (exec this state)))

(defrecord Mul
  [X Y]
  Instr
  (exec [_ state]
    (let [^longs state state
          regX (.resolveRegister ^Arg X)
          vY (.resolveValue ^Arg Y state)]
      (aset state
        regX
        (* (aget state regX) vY))
      (inc-code-pointer! state)
      nil))
  (exec2 [this state _ _]
    (exec this state)))

(defrecord Mod
  [X Y]
  Instr
  (exec [_ state]
    (let [^longs state state
          regX (.resolveRegister ^Arg X)
          vY (.resolveValue ^Arg Y state)]
      (aset state
        regX
        (long (mod (aget state regX) vY)))
      (inc-code-pointer! state)
      nil))
  (exec2 [this state _ _]
    (exec this state)))

(defrecord Rcv
  [X]
  Instr
  (exec [_ state]
    (let [^longs state state
          vX (.resolveValue ^Arg X state)]
      (inc-code-pointer! state)
      (if (zero? vX)
        nil
        (aget state 1))))
  (exec2 [_ state _ rcv!]
    (let [^longs state state
          regX (.resolveRegister ^Arg X)]
      (aset state regX
        (long (rcv!)))
      (inc-code-pointer! state))))

(defrecord Jgz
  [X Y]
  Instr
  (exec [_ state]
    (let [^longs state state
          vX (.resolveValue ^Arg X state)
          vY (.resolveValue ^Arg Y state)]
      (aset state 0
        (+ (aget state 0)
          (if (> vX 0)
            vY
            (long 1))))
      nil))
  (exec2 [this state _ _]
    (exec this state)))

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

  (->> input
    (str/split-lines)
    (map str/trim)
    (map-indexed vector)
    vec)
  =>
  [[0 "set i 31"]
   [1 "set a 1"]
   [2 "mul p 17"]
   [3 "jgz p p"]
   [4 "mul a 2"]
   [5 "add i -1"]
   [6 "jgz i -2"]
   [7 "add a -1"]
   [8 "set i 127"]
   [9 "set p 826"]
   [10 "mul p 8505"]
   [11 "mod p a"]
   [12 "mul p 129749"]
   [13 "add p 12345"]
   [14 "mod p a"]
   [15 "set b p"]
   [16 "mod b 10000"]
   [17 "snd b"]
   [18 "add i -1"]
   [19 "jgz i -9"]
   [20 "jgz a 3"]
   [21 "rcv b"]
   [22 "jgz b -1"]
   [23 "set f 0"]
   [24 "set i 126"]
   [25 "rcv a"]
   [26 "rcv b"]
   [27 "set p a"]
   [28 "mul p -1"]
   [29 "add p b"]
   [30 "jgz p 4"]
   [31 "snd a"]
   [32 "set a b"]
   [33 "jgz 1 3"]
   [34 "snd b"]
   [35 "set f 1"]
   [36 "add i -1"]
   [37 "jgz i -11"]
   [38 "snd a"]
   [39 "jgz f -16"]
   [40 "jgz a -19"]]

  )

(defn init-state1
  [compiled]
  (long-array (+ 2 (count (:registers compiled))) 0))

(defn exec-next
  [^objects instrs, ^longs state]
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
        state (init-state1 compiled)]
    (find-recovered instrs state)))

(comment
  (solve1 compiled)
  => 7071
  )

;--- Part Two ---
;As you congratulate yourself for a job well done, you notice that the documentation has been
; on the back of the tablet this entire time. While you actually got most of the instructions correct,
; there are a few key differences. This assembly code isn't about sound at all - it's meant to be run
; twice at the same time.
;
;Each running copy of the program has its own set of registers and follows the code independently -
; in fact, the programs don't even necessarily run at the same speed. To coordinate,
; they use the send (snd) and receive (rcv) instructions:
;
;snd X sends the value of X to the other program. These values wait in a queue until that program is
; ready to receive them. Each program has its own message queue, so a program can never receive a message
; it sent.
;rcv X receives the next value and stores it in register X. If no values are in the queue, the program
; waits for a value to be sent to it. Programs do not continue to the next instruction until they have received a value.
; Values are received in the order they are sent.
;Each program also has its own program ID (one 0 and the other 1); the register p should begin with
; this value.
;
;For example:
;
;snd 1
;snd 2
;snd p
;rcv a
;rcv b
;rcv c
;rcv d
;Both programs begin by sending three values to the other. Program 0 sends 1, 2, 0; program 1
; sends 1, 2, 1. Then, each program receives a value (both 1) and stores it in a,
; receives another value (both 2) and stores it in b,
; and then each receives the program ID of the other program (program 0 receives 1; program 1 receives 0)
; and stores it in c. Each program now sees a different value in its own copy of register c.
;
;Finally, both programs try to rcv a fourth time, but no data is waiting for either of them, and they reach a deadlock.
; When this happens, both programs terminate.
;
;It should be noted that it would be equally valid for the programs to run at different speeds;
; for example, program 0 might have sent all three values and then stopped at the first rcv before program 1 executed
; even its first instruction.
;
;Once both of your programs have terminated (regardless of what caused them to do so),
; how many times did program 1 send a value?

;; there's one additional register counting the sent values

(defn init-state2
  [compiled pid]
  (doto
    (long-array (+ 2 (count (:registers compiled))) 0)
    (aset (get-in compiled [:register-names 'p]) (long pid))))

(defn solve2
  [compiled]
  (let [r (atom [{:activity :running
                  :sent 0}
                 {:activity :running
                  :sent 0}])
        queues [(LinkedBlockingDeque.)
                (LinkedBlockingDeque.)]
        p-terminated (promise)]
    (add-watch r
      ::watch-terminated
      (fn [_ _ _ [{a0 :activity}
                  {a1 :activity}
                  :as rv]]
        (when (and
                (not= a0 :running)
                (not= a1 :running))
          (future
            ;; HACK reducing the risk for inconsistent reads by ensuring the situation stabilizes
            (Thread/sleep 100)
            (let [rv2 @r]
              (when (identical? rv2 rv)
                (deliver p-terminated rv)))))))
    (doseq [pid [0 1]]
      (future
        (let [^BlockingDeque q-send (get queues pid)
              ^BlockingDeque q-receive (get queues (- 1 pid))
              ^longs state (init-state2 compiled pid)
              snd! (fn [v]
                     (swap! r
                       (fn [rv]
                         (supd/supdate rv
                           {pid
                            {:sent inc}
                            (- 1 pid)
                            {:activity
                             #(if (= % :receiving)
                                :running
                                %)}})))
                     (.addLast q-send v)
                     #_(println (str "process " pid " sent " v)))
              rcv! (fn []
                     (let [v
                           (if (-> q-receive .size (> 0))
                             (.removeFirst q-receive)
                             (do
                               (swap! r
                                 (fn [rv]
                                   (supd/supdate rv
                                     {pid {:activity (constantly :receiving)}})))
                               (.takeFirst q-receive)))]
                       #_(println (str "process " pid " received " v))
                       v))
              ^objects instrs (:instrs compiled)]
          (loop [t 0]
            (let [code-i (aget state 0)]
              #_(println (str "pid " pid ": instr " code-i))
              (if (< code-i (alength instrs))
                (let [instr (aget instrs code-i)]
                  (try
                    (exec2 instr state snd! rcv!)
                    (catch Throwable err
                      (deliver p-terminated
                        (ex-info
                          (str "Process failed")
                          {:pid pid :code-i code-i :instr instr
                           :state (vec state)}
                          err))
                      (throw err)))
                  (recur (inc t)))
                (swap! r
                  #(supd/supdate % {pid {:activity (constantly :completed)}}))))))))
    (let [ret @p-terminated]
      (if (instance? Throwable ret)
        (throw ret)
        (get-in ret [1 :sent])))))

(comment
  ;; NOTE not deterministic, so there must be race conditions above.
  ;; however, the result is 8001 in the vast majority of cases
  (solve2 compiled)
  => 8001

  (->> (repeatedly 100 #(solve2 compiled))
    frequencies)
  => {8001 98, 5226 1, 5922 1}
  )



