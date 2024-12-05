(ns aoc.day-5
  (:require [aoc]
            [clojure.set :as set]))

;; # Advent of Code - Day 5

(def input
  (aoc/input 5 :lines true :ints true))

;; ## Part 1

(defn parse-input
  [input]
  (let [[rules _ orders] (partition-by empty? input)]
    {:rules
     (reduce
       (fn [acc [before-page after-page]]
         (update acc before-page (fnil conj #{}) after-page))
       {}
       rules)
     :updates orders}))

(defn correct-order?
  "Return whether the `order` is in the correct order"
  [{:keys [rules]} order]
  (loop [[page & rest-pages] order]
    (if-not rest-pages
      true
      (let [remaining-pages (set rest-pages)]
        (if-not (set/subset? remaining-pages (rules page))
          false
          (recur rest-pages))))))


(defn middle-page
  "Return the middle page of the provided order"
  [order]
  (assert (mod (count order) 2) "Order must have an odd number of pages")
  (nth order (/ (count order) 2)))


;; Solve part one

(let [{:keys [updates] :as puzzle} (parse-input input)]
  (->> updates
       (filter #(correct-order? puzzle %))
       (map middle-page)
       (reduce +)))


;; ## Part 2

(defn fix-order
  "Fix the ordering of the provided `incorrect-order`"
  [{:keys [rules]} incorrect-order]
  (loop [remaining-pages (set incorrect-order)
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

(let [{:keys [updates] :as puzzle} (parse-input input)]
  (->> updates
       (remove #(correct-order? puzzle %))
       (map (comp middle-page (partial fix-order puzzle)))
       (reduce +)))
