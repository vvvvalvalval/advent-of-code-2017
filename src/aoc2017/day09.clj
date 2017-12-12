(ns aoc2017.day09
  (:require [instaparse.core :as insta]))

(def ebnf
  "group = '{' (GROUP_ITEM (',' GROUP_ITEM)* )? '}'
  GROUP_ITEM = group | garbage
  garbage = garbage_open (garbage_cancel | garbage_regular)* garbage_close
  garbage_open = '<'
  garbage_close = '>'
  garbage_cancel = '!' #'.'
  garbage_regular = #'[^>!]'
  ")

(def ip (insta/parser ebnf :output-format :enlive))

(def parse
  (letfn [(group [p]
            (update p :content
              (fn [content]
                (into []
                  (comp
                    (remove string?)
                    (map group-item))
                  content))))
          (group-item [p]
            (let [i (first (:content p))]
              (case (:tag i)
                :group (group i)
                :garbage (garbage i))))
          (garbage [p]
            (update p :content #(apply str %)))]
    (fn [input]
      (group (ip input)))))

(comment
  (parse "{{<a>},{<a>},{<a>},{<a>}}"))

(defn score
  [parsed]
  (letfn [(aux [depth group]
            (apply +
              depth
              (->> group :content
                (filter #(-> % :tag (= :group)))
                (map (partial aux (inc depth))))))]
    (aux 1 parsed)))

(comment

  (def input "")


  (score (parse input))
  )


(defn count-garbage
  [input]
  (letfn [(aux [p]
            (cond
              (string? p)
              0
              (map? p)
              (if (-> p :tag (= :garbage_regular))
                1
                (apply +
                  (map aux (:content p))))))]
    (let [p (ip input)]
      (aux p))))

(count-garbage input)