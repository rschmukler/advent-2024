(ns aoc.day-11
  (:require [aoc]))

(defn to-digits
  [x]
  (loop [result nil
         x      x]
    (if (< x 10)
      (cons x result)
      (let [digit (rem x 10)]
        (recur (cons digit result) (quot x 10))))))

(defn digits-to-int
  [digits]
  (loop [grp    1
         digits (reverse digits)
         result 0]
    (if-some [d (first digits)]
      (recur
        (* 10 grp)
        (rest digits)
        (+ result (* grp d)))
      result)))

(defn blink-at-stone
  [stone]
  (let [digits (delay (to-digits stone))]
    (cond
      (zero? stone)           (list 1)
      (even? (count @digits)) (map digits-to-int (split-at (/ (count @digits) 2) @digits))
      :else                   (list (* stone 2024)))))


(defn blink-at-stones
  [stones]
  (mapcat blink-at-stone stones))

(defn count-after-n-blinks
  [n stones]
  (->> (iterate blink-at-stones stones)
       (take (inc n))
       (last)
       (count)))

;; Part one

(def input
  (aoc/input 11 {:ints true}))

(defn blink-at-stones-fast
  [stone-counts]
  (reduce
    (fn [new-stone-counts stone]
      (let [[stone-a stone-b] (blink-at-stone stone)
            mult              (stone-counts stone)]
        (cond-> new-stone-counts
          true    (update stone-a (fnil + 0) mult)
          stone-b (update stone-b (fnil + 0) mult))))
    {}
    (keys stone-counts)))


(defn count-after-n-blinks-fast
  [n stones]
  (->> (iterate blink-at-stones-fast (frequencies stones))
       (drop n)
       (first)
       (vals)
       (reduce +)))


(count-after-n-blinks-fast 75 input)
