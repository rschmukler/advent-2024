(ns aoc.day-8
  {:nextjournal.clerk/visibility {:result :hide}}
  (:require [aoc]
            [nextjournal.clerk :as clerk]))

;; # Advent of Code - Day 8
;; ## Part one
(def puzzle-input
  "Our puzzles input"
  (aoc/input 8 :lines true))


(defn input->layout
  "Parse the puzzle input into a map of the layout"
  [input]
  (let [y (count input)
        x (count (first input))]
    {:size [x y]
     :antennas
     (reduce
       (fn [acc [freq pos]]
         (update acc freq (fnil conj #{}) pos))
       {}
       (for [[y line] (map-indexed vector input)
             [x c]    (map-indexed vector line)
             :when    (not= \. c)]
         [c [x y]]))}))

(defn in-bounds?
  "Return whether the given position is in bounds"
  [bounds pos]
  (let [[bx by] bounds
        [x y]   pos]
    (and (<= 0 x (dec bx))
         (<= 0 y (dec by)))))


(defn antinodes-between-antennas
  "Return a set of any antinodes that would occur between two theoretical antennas `a` and `b` in
  the given `bounds`"
  [bounds a b]
  (let [[ax ay]            a
        [bx by]            b
        dx                 (- bx ax)
        dy                 (- by ay)
        antinodes-between? (and (zero? (rem dx 3))
                                (zero? (rem dy 3)))
        nodes              (->> (if antinodes-between?
                                  (let [max-x  (max ax bx)
                                        min-x  (min ax bx)
                                        max-y  (max ay by)
                                        min-y  (min ay by)
                                        x-step (quot (abs dx) 3)
                                        y-step (quot (abs dy) 3)]
                                    (->> (iterate (fn [[x y]] [(+ x x-step) (+ y y-step)]) [min-x min-y])
                                         (rest)
                                         (take-while #(not= % [max-x max-y]))))
                                  [[(+ bx dx) (+ by dy)]
                                   [(- ax dx) (- ay dy)]])
                                (filter #(in-bounds? bounds %)))]
    (when (seq nodes)
      (set nodes))))


(defn find-antinodes-on-frequency
  "Return a set of antinodes between the given `antenna-set` in the given `bounds`.

  Uses the provided `f` as the function to find antinodes between antennas"
  [bounds antenna-set f]
  (loop [[a & as] antenna-set
         result   #{}
         seen     #{}]
    (if-not a
      result
      (let [pairings  (map #(set [%1 %2]) (repeat a) (remove #{a} antenna-set))
            new-nodes (->> pairings
                           (remove seen)
                           (mapcat #(apply f bounds %)))]
        (recur
          as
          (into result new-nodes)
          (into seen pairings))))))

(defn find-antinodes-on-map
  "Return all unique antinodes on the given map using the provided `f` to find antinodes
  between antennas"
  [{:keys [antennas size]} f]
  (->> (for [[_ antennas-on-freq] antennas
             antinode             (find-antinodes-on-frequency size antennas-on-freq f)]
         antinode)
       (set)))


;; Solve part one

^{:nextjournal.clerk/visibility {:result :show}}
(-> puzzle-input
    (input->layout)
    (find-antinodes-on-map antinodes-between-antennas)
    count)


;; ## Part 2

;; I refactored all of our functions to take an argument `f` which represents the calculation of
;; antinodes between two antennas.
;;
;; To me, part 2 feels easier than part one. We basically want to find the GCD of the deltas and use
;; that to trace a path in the map between the two antennas.

(defn antinodes-between-antennas-part-two
  "Function to find antinodes between antennas per the instructions in part two"
  [bounds a b]
  (let [[ax ay] a
        [bx by] b
        dx      (- bx ax)
        dy      (- by ay)
        gcd     (aoc/gcd (abs dx) (abs dy))
        dx-sm   (/ dx gcd)
        dy-sm   (/ dy gcd)]
    (set
      (concat
        (->> (iterate (fn [[x y]] [(+ x dx-sm) (+ y dy-sm)]) a)
             (take-while #(in-bounds? bounds %)))
        (->> (iterate (fn [[x y]] [(- x dx-sm) (- y dy-sm)]) b)
             (take-while #(in-bounds? bounds %)))))))

;; Solve part two

^{:nextjournal.clerk/visibility {:result :show}}
(-> puzzle-input
    (input->layout)
    (find-antinodes-on-map antinodes-between-antennas-part-two)
    count)
;; ## Visualization

;; While developing the above I ran into issues with incorrect items. I wrote the following function
;; to aid with development:

(defn render-map
  [{:keys [size antennas]} f]
  (let [coord->antenna      (into
                              {}
                              (for [[c antennas] antennas
                                    a            antennas]
                                [a c]))
        antenna->color      (into {} (map vector (sort (keys antennas)) ["blue"
                                                                         "green"
                                                                         "indigo"
                                                                         "red"
                                                                         "orange"]))
        coord->antinode-src (into
                              {}
                              (for [[c antennas] antennas
                                    antinode     (find-antinodes-on-frequency size antennas f)]
                                [antinode c]))]
    [:div
     (into
       [:div
        {:style {:height                "600px"
                 :width                 "600px"
                 :grid-template-columns (format "repeat(%d, 1fr)" (inc (first size)))}
         :class ["grid" "m-10"]}]
       (for [y    (range -1 (first size))
             x    (range -1 (second size))
             :let [coord [x y]]]
         [:div
          {:class ["border" "bg-gray-50" "flex" "items-center" "justify-center"
                   "text-gray-700/80" "font-sans" "text-sm" "font-bold"
                   (some->> coord coord->antenna antenna->color
                            (format "bg-%s-100"))
                   (some->> coord coord->antinode-src antenna->color
                            (format "bg-%s-100"))
                   (when (neg? y)
                     "border-b-gray-500")
                   (when (neg? x)
                     "border-r-gray-500")]}
          (when (and (neg? y) (not (neg? x)))
            [:span {:class ["text-gray-400"]}
             x])
          (when (and (neg? x) (not (neg? y)))
            [:span {:class ["text-gray-400"]}
             y])
          (when-some [a (coord->antenna coord)]
            (str a))])
       )]))

^{:nextjournal.clerk/visibility {:result :show}}
(clerk/with-viewer
  clerk/html
  (-> (aoc/example 8 :lines true)
      (input->layout)
      (render-map antinodes-between-antennas)))
