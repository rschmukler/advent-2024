(ns aoc.day-4
  (:require [aoc]))

;; # Advent of Code - Day 4

;; Start by grabbing our puzzle input
(def input
  (aoc/input 4 :lines true))

(defn- -trace-path
  "Utility function to traverse from `pos` along the given delta returning the path of coordinates
  that matched the word, or `nil` if it doesn't hit"
  [coord->char pos [dx dy] word]
  (loop [word          word
         [x y :as pos] pos
         result        []]
    (cond
      (empty? word)                      result
      (= (coord->char pos) (first word)) (recur (rest word)
                                                [(+ x dx) (+ y dy)]
                                                (conj result pos))
      :else                              nil)))

(defn find-paths
  "Retunr all valid paths that spell the `word` in the given puzzle input"
  [puzzle-input word]
  (let [coord->char (->> (for [[y line] (map-indexed vector puzzle-input)
                               [x c]    (map-indexed vector line)]
                           [[x y] c])
                         (into {}))]
    (for [[pos c] coord->char
          :when   (= c (first word))
          dx      (range -1 2)
          dy      (range -1 2)
          :when   (not (and (zero? dx) (zero? dy)))
          :let    [delta [dx dy]
                   path (-trace-path coord->char pos delta word)]
          :when   path]
      path)))

;; Solve part one
(count (find-paths input "XMAS"))


;; ## Part 2
;; Let's write a few more functions to help us see if we have an X

(def diagonal-delta?
  "Return whether the provided `delta` is diagonal"
  #{[1 1] [-1 -1] [1 -1] [-1 1]})

(defn path->delta
  "Return the delta of the provided path"
  [p]
  (let [[x1 y1] (first p)
        [x2 y2] (second p)]
    [(- x2 x1)
     (- y2 y1)]))

(defn forms-x?
  [p1 p2]
  (and (= (nth p1 1)
          (nth p2 1))
       (-> p1 path->delta diagonal-delta?)
       (-> p2 path->delta diagonal-delta?)))

;; Naive O(n^2) attempt. Take all paths of `MAS` and see if they form an X.
;; Then, return distinct ones (because all Xs will appear 2x otherwise)
(count
  (let [paths (find-paths input "MAS")]
    (->>  (for [p1    paths
                p2    paths
                :when (and (not= p1 p2)
                           (forms-x? p1 p2))]
            #{p1 p2})
          (distinct))))

;; This ended up being fast enough, but another way to potentially solve the
;; puzzle would have been doing a mask of all the X-MAS shapes and then seeing the overlap on
;; every A in the puzzle (O(n))
