(ns aoc.day-3
  (:require [aoc]
            [clojure.string :as str]))

;; # Advent of Code - Day 3

;; Let's start by loading the puzzle input
(defonce input
  (aoc/input 3))

; ## Part 1

(defn parse-part-one-instructions
  "Parse the provided input for a list of corrupted multiplication instructions.
  Return tuples in the form of [:mul x y]"
  [input]
  (for [[_ x y] (re-seq #"mul\((\d+),(\d+)\)" input)]
    [:mul (parse-long x) (parse-long y)]))

;; Test it out on the example input
(parse-part-one-instructions
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

;; Solve part one:

(->> input
     (parse-part-one-instructions)
     (map (fn [[_ x y]] (* x y)))
     (reduce +))


; ## Part 2

;; Write a nifty regex for matching on the different instruction types. Rely on position of capture
;; groups to make it easy to parse the results.

(defn parse-part-two-instructions
  [input]
  (for [[_ mul? x y do dont] (re-seq #"(mul)\((\d+),(\d+)\)|(do)\(\)|(don't)\(\)" input)]
    (cond
      mul?
      [:mul (parse-long x) (parse-long y)]
      do
      [:do]
      dont
      [:dont])))

;; Test it out on the example input
(parse-part-two-instructions
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

;; Solve part two

(->> input
     parse-part-two-instructions
     (reduce
       (fn [[sum enabled?] [cmd x y]]
         (case cmd
           :mul  [(+ sum (if-not enabled? 0 (* x y))) enabled?]
           :do   [sum true]
           :dont [sum false]))
       [0 true])
     first)
