(ns aoc2016.day10
  (:require [clara.rules :as c]
            [aoc2017.utils :as u]
            [clojure.string :as str]))

(defrecord ValueGoesToBot [bot value])

(defrecord BotHasValues [bot low high])

(defrecord BotGivesTo [bot low-dest high-dest])

(c/defrule r1
  [ValueGoesToBot (= bot ?bot) (= value ?low)]
  [ValueGoesToBot (= bot ?bot) (= value ?high)]
  [:test (< ?low ?high)]
  =>
  (c/insert-all!
    [(->BotHasValues ?bot ?low ?high)]))

(c/defrule r2
  [BotHasValues (= bot ?bot) (= low ?low) (= high ?high)]
  [BotGivesTo (= bot ?bot) (= low-dest ?low-dest) (= high-dest ?high-dest)]
  =>
  (c/insert-all!
    [(->ValueGoesToBot ?low-dest ?low)
     (->ValueGoesToBot ?high-dest ?high)]))

(c/defquery bot-values
  [:?bot]
  [?bhv <- BotHasValues (= ?bot bot)])

(c/defquery bot-who-compares
  [:?low :?high]
  [?bhv <- BotHasValues (= ?bot bot) (= ?low low) (= ?high high)])

(defn parse-line
  [l]
  (if-let [[_ vs bs]
           (re-matches
             #"value.(\d+).goes.to.(bot.\d+)"
             l)]
    (->ValueGoesToBot bs (u/parse-long vs))
    (if-let [[_ src low-dest _ high-dest _]
             (re-matches
               #"(bot.\d+).gives.low.to.((bot|output).\d+).and.high.to.((bot|output).\d+)"
               l)]
      (->BotGivesTo src low-dest high-dest)
      (throw (ex-info "Failed to parse line" {l l})))))

(defn parse-input
  [input]
  (->> input
    str/split-lines
    (mapv parse-line)))

(parse-input
  "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2")

(def input "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2")
(def lw 3)
(def h 5)

(defn bot-number
  [^String bot-s]
  (let [[_ n] (re-matches #"bot.(\d+)" bot-s)]
    (u/parse-long n)))

(defn populate [input]
  (-> (c/mk-session 'aoc2016.day10)
    (c/insert-all
      (parse-input input))
    (c/fire-rules)))

(defn part1
  [input low high]
  (-> (populate input)
    (c/query bot-who-compares :?low low :?high high)
    first
    :?bot
    bot-number))

(c/defquery outputs-*
  []
  [ValueGoesToBot (= bot "output 0") (= value ?v1)]
  [ValueGoesToBot (= bot "output 1") (= value ?v2)]
  [ValueGoesToBot (= bot "output 2") (= value ?v3)])

(defn part2
  [input]
  (let [{:keys [?v1 ?v2 ?v3]}
        (-> (populate input)
          (c/query outputs-*)
          first)]
    (* ?v1 ?v2 ?v3)))

(comment
  (part1 input lw h))


(comment
  (-> (c/mk-session 'aoc2016.day10)
    (c/insert-all
      [(->ValueGoesToBot "bot 1" 3)
       (->ValueGoesToBot "bot 1" 5)])
    (c/fire-rules)
    (c/query bot-values :?bot "bot 1")))



