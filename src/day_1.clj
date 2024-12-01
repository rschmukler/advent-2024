(ns day-1
  (:require [clojure.string :as str]))

(defonce puzzle-input
  (slurp "resources/day_1/input.txt"))

(defn parse-puzzle-input
  "Parse the puzzle input into a tuple of the two lists"
  [txt]
  (reduce
    (fn [[as bs] line]
      (let [[a b] (re-seq #"\d+" line)]
        [(conj as (parse-long a))
         (conj bs (parse-long b))]))
    [[][]]
    (str/split-lines txt)))

(defn compute-total-distance
  "Compute the total distance between the two lists"
  [[as bs]]
  (reduce
    +
    (map
      #(Math/abs (- %1 %2))
      (sort as)
      (sort bs))))

(defn compute-total-similarity-score
  "Compute the total similarity score between the two lists"
  [[as bs]]
  (let [a->b-cnt (frequencies bs)]
    (reduce + (map #(* % (a->b-cnt % 0)) as))))

(comment
  ;; Solve part one
  (-> puzzle-input
      parse-puzzle-input
      compute-total-distance)

  (-> puzzle-input
      parse-puzzle-input
      compute-total-similarity-score))
