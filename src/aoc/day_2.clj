(ns aoc.day-2
  (:require [aoc]))

(defonce puzzle-input
  (aoc/input 2 {:lines true :ints true}))

(defn safe?
  "Return whether the provided report is safe"
  [report]
  (let [distances (->> report
                       (partition 2 1)
                       (map (partial apply (comp abs -))))]
    (and (or (apply < report)
             (apply > report))
         (<= (apply max distances) 3))))

;; Solve part one
(->> puzzle-input
     (filter safe?)
     count)

(defn safe-removing-one?
  "Return whether the provided report, removing up to one level
  is safe"
  [report]
  (->> (for [drop-ix (range (count report))
             :let [report-v (vec report)
                   new-report (into
                                (subvec report-v 0 drop-ix)
                                (subvec report-v (inc drop-ix) (count report-v)))]]
         new-report)
       (some safe?)))

;; Solve part two
(->> puzzle-input
     (filter safe-removing-one?)
     count)
