(ns aoc.day-7
  {:nextjournal.clerk/visibility {:result :hide}}
  (:require [aoc]
            [nextjournal.clerk :as clerk]))

;; # Advent of Code - Day 7

^{:nextjournal.clerk/visibility {:result :show}}
(def puzzle-input
  (aoc/input 7 {:lines true :ints true}))


;; ## Part 1

;; First, let's get a sense of how many operations it would take to solve part one just by trying
;; every option. To this we will take the number of numbers per line and subtract 1 to show
;; the number of potential binary forks (between `+` or `*`).

^{:nextjournal.clerk/visibility {:result :show}}
(->> puzzle-input
     (map (comp int #(Math/pow 3 %) dec count rest))
     (reduce +))


;; Nice, this is small enough that we can just brute force it.

(defn interprit-sequence
  "Return the value from the provided sequence"
  [s]
  (loop [result (first s)
         items  (rest s)]
    (if (empty? items)
      result
      (let [[op num] (take 2 items)]
        (recur
          (case op
            * (* result num)
            + (+ result num)
            | (parse-long (str result num)))
          (drop 2 items))))))

(defn permute-sequence
  "Permute the sequence of numbers"
  [nums {:keys [ops] :as opts}]
  (if (= 1 (count nums))
    (list (list (first nums)))
    (for [op     ops
          option (permute-sequence (rest nums) opts)]
      (lazy-cat (list (first nums) op)
                option))))

(defn solve-operation-sequence
  "Return possible solutions to get `tgt` using `nums`"
  [tgt nums opts]
  (->> (permute-sequence nums opts)
       (filter (comp #{tgt} interprit-sequence))
       seq))


^{:nextjournal.clerk/visibility {:result :show}}
;; Using the above lets solve for part 1
(->> puzzle-input
     (keep #(when (solve-operation-sequence (first %) (rest %) {:ops '[+ *]})
              (first %)))
     (reduce +))


;; ## Part 2

(comment
  (time
    (->> puzzle-input
         (pmap #(when (solve-operation-sequence (first %) (rest %) {:ops '[+ * |]})
                  (first %)))
         (filter some?)
         (reduce +))))

;; Extending out `solve-operation-sequence` to take a list of `ops` to try, and
;; extending `interprit-sequence` to be able to use `|` ended up being performant enough
;; with some `pmap`ing. I comment it out to save the GitHub actions runner, but it finished in
;; 38.809s on my machine


;; ## Optimizing part 2

;; While we were able to brute-force the solution, it defenitely felt slower than it needed to be.
;; Let's try and implement a more optimal solution by writing a function that will actually apply
;; the operations while calculating.

;; We will define the operations supported as a map of `symbol` to `op-fn` where `op-fn` is an
;; arity 2 function that will apply two numbers and return the result. We can use `*` and `+` for
;; their respective symbols, but let's define a `pipe-op` fn for the pipe operator described in part
;; two.

(defn pipe-op
  "Implementation for the `||` operator"
  [a b]
  (parse-long (str a b)))

(def part-one-ops
  "The set of operations in part one"
  {'+ + '* *})

(def part-two-ops
  "The set of operations in part two"
  (assoc part-one-ops '|| pipe-op))

(defn solve-operation-sequence-fast
  "An optimized version of `solve-operation-sequence` which applies the operations to the numbers"
  [tgt [a b :as nums] ops]
  (cond
    (and (= a tgt) (nil? b))    (list nil)
    (and (not= a tgt) (nil? b)) nil
    :else
    (seq
      (for [;; Iterate over every operation
            [op-sym op-f] ops
            ;; Iterate over every way to make the tgt number on the rest of the list
            ;; with the `op-f` applied to `a` and `b`
            s             (solve-operation-sequence-fast
                            tgt
                            (cons (op-f a b) (rest (rest nums))) ops)]
        (concat (list a op-sym b) (rest s))))))

{::clerk/visibility {:result :show}}
(solve-operation-sequence-fast 3 '(1 2) part-one-ops)
(solve-operation-sequence-fast 6 '(1 2 3) part-one-ops)
(solve-operation-sequence-fast 3267 '(81 40 27) part-one-ops)
(solve-operation-sequence-fast 7290 '(6 8 6 15) part-two-ops)

;; Let's compare the timing on these two
^{::clerk/visibility {:result :hide}}
(comment
  (time
    (->> puzzle-input
         (keep #(when (solve-operation-sequence (first %) (rest %) {:ops '[+ *]})
                  (first %)))
         (reduce +)))
  ;; => 1264.55ms

  (time
    (->> puzzle-input
         (keep #(when (solve-operation-sequence-fast (first %) (rest %) part-one-ops)
                  (first %)))
         (reduce +)))
  ;; => 47.675ms
  )

;; On my laptop we go from `1264.55ms` to `47.675ms`. 26x speed up!

;; How about on the solution to part two?
(time
  (->> puzzle-input
       (pmap #(when (solve-operation-sequence-fast (first %) (rest %) part-two-ops)
                (first %)))
       (filter some?)
       (reduce +)))

;; Done in `456ms` compared to `38.809s` - 128x faster. Nice!
