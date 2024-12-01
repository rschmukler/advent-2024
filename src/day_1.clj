(ns day-1
  (:require [aoc]))

(defonce puzzle-input
  (aoc/input 1))

(defn parse-puzzle-input
  "Parse the puzzle input into a tuple of the two lists"
  [txt]
  (-> txt
      (aoc/process-input {:lines true :ints true})
      (aoc/transpose)))

(defn compute-total-distance
  "Compute the total distance between the two lists"
  [[as bs]]
  (reduce
    +
    (map
      #(abs (- %1 %2))
      (sort as)
      (sort bs))))

(defn compute-total-similarity-score
  "Compute the total similarity score between the two lists"
  [[as bs]]
  (let [a->b-cnt (frequencies bs)]
    (reduce + (map #(* % (a->b-cnt % 0)) as))))

;; Solve part one
(-> puzzle-input
    parse-puzzle-input
    compute-total-distance)

;; Solve part two
(-> puzzle-input
    parse-puzzle-input
    compute-total-similarity-score)
