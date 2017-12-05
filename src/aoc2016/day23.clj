(ns aoc2016.day23
  (:require [vvvvalvalval.supdate.api :as supd]
            [clojure.string :as str]
            [aoc2017.utils :as u]))

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

(defn inc*
  [ms x]
  {:pre [(string? x)]}
  (supd/supdate ms
    {:i inc
     :registers {x inc}}))

(defn dec*
  [ms x]
  {:pre [(string? x)]}
  (supd/supdate ms
    {:i inc
     :registers {x dec}}))

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

(defmulti exec-instruction
  (fn [ms [op & args]] op))

(defmethod exec-instruction :cpy
  [ms [_ x y]] (cpy* ms x y))

(defmethod exec-instruction :inc
  [ms [_ x]] (inc* ms x))

(defmethod exec-instruction :dec
  [ms [_ x y]] (dec* ms x))

(defmethod exec-instruction :jnz
  [ms [_ x y]] (jnz* ms x y))

(defn exec-instr
  [ms instr]
  (try
    (exec-instruction ms instr)
    (catch Throwable err
      (prn err)
      (update ms :i inc))))

(defn run
  [code registers]
  (loop [ms (init-state code registers)]
    #_(prn "a" (-> ms :registers (get "a")))
    (if (terminated? ms)
      ms
      (recur (exec-instr ms (get (:code ms) (:i ms)))))))

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
  (def input "")

  (future
    (run
      (parse input)
      (assoc (zipmap ["a" "b" "c" "d"] (repeat 0))
        "a" 12))))



