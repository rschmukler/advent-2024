(ns aoc.day-5
  (:require [aoc]
            [clojure.set :as set]))

;; # Advent of Code - Day 5

(def input
  (aoc/input 5 :lines true :ints true))

;; ## Part 1

;; We will start by parsing the input into a data structure that represents the puzzle.
;; The `:rules` map will contain a mapping of page number to the set of pages that come after it.
(defn input->puzzle
  [input]
  (let [[rules _ orders] (partition-by empty? input)]
    {:rules
     (reduce
       (fn [acc [before-page after-page]]
         (update acc before-page (fnil conj #{}) after-page))
       {}
       rules)
     :updates orders}))

(def puzzle
  (input->puzzle input))

(defn correct-order?
  "Return whether the `update-order` is in the correct order"
  [{:keys [rules]} update-order]
  (loop [[page & rest-pages] update-order]
    (if-not rest-pages
      true
      (let [remaining-pages (set rest-pages)]
        ;; Cheeck whether all the remaining pages are a subset of the rules of pages that come after
        (if-not (set/superset? (rules page) remaining-pages)
          false
          (recur rest-pages))))))


(defn middle-page
  "Return the middle page of the provided order"
  [update-order]
  (assert (mod (count update-order) 2) "Order must have an odd number of pages")
  (nth update-order (/ (count update-order) 2)))


;; Solve part one

(->> (:updates puzzle)
     (filter #(correct-order? puzzle %))
     (map middle-page)
     (reduce +))


;; ## Part 2

;; We need to write a function to fix the ordering of the provided update in `incorrect-update`.
;; We will do this by iterating over page numbers to find the page with a rule that has all other
;; pages comining after it. Once we have it, add it to the result and repeat the process with
;; the remaining pages.

(defn fix-order
  "Fix the ordering of the provided `incorrect-update`"
  [{:keys [rules]} incorrect-update]
  (loop [remaining-pages (set incorrect-update)
         attempted-pages #{}
         result          '()]
    (if (empty? remaining-pages)
      (reverse result)
      (let [trial-page (first (remove attempted-pages remaining-pages))]
        (if (set/superset? (rules trial-page) (disj remaining-pages trial-page))
          (recur (disj remaining-pages trial-page)
                 #{}
                 (cons trial-page result))
          (recur remaining-pages
                 (conj attempted-pages trial-page)
                 result))))))

;; Solve part two

(->> (:updates puzzle)
     (remove #(correct-order? puzzle %))
     (map (comp middle-page (partial fix-order puzzle)))
     (reduce +))
