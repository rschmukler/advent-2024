(ns aoc.day-6
  (:require [aoc]
            [nextjournal.clerk :as clerk]))

; # Advent of Code - Day 6

(defonce input
  (aoc/input 6 :lines true))


(defn input->patrol-map
  "Parse the input into a patrol map"
  [input]
  (reduce
    (fn [acc [x y c]]
      (case c
        \# (update acc :obstacles conj [x y])
        \^ (assoc acc :guard {:path [[x y]] :direction :up})
        acc))
    {:obstacles #{}
     :size      [(count (first input))
                 (count input)]}
    (for [[y line] (map-indexed vector input)
          [x c]    (map-indexed vector line)]
      [x y c])))

(defn turn-guard-90-deg
  "Turn the guard in the patrol map 90 degrees to the right"
  [guard]
  (update guard :direction {:up    :right
                            :right :down
                            :down  :left
                            :left  :up}))

(defn walk-in-direction
  "Return the next coordinate in the provided direction"
  [[x y] direction]
  (let [[dx dy] (case direction
                  :up    [0 -1]
                  :right [1 0]
                  :down  [0 1]
                  :left  [-1 0])]
    [(+ x dx) (+ y dy)]))

(defn in-bounds?
  [{:keys [size]} [x y]]
  (and (< -1 x (first size))
       (< -1 y (second size))))

(defn out-of-bounds?
  [patrol-map pos]
  (not (in-bounds? patrol-map pos)))

(defn walk-guard-until-obstacle-or-out-of-bounds
  "Walk the guard in the patrol map until it hits an obstacle or goes out of bounds"
  [{:keys [guard obstacles] :as patrol-map}]
  (loop [{:keys [direction path] :as guard} guard]
    (let [guard-pos (last path)
          next-pos  (walk-in-direction guard-pos direction)]
      (cond
        (obstacles next-pos)                 (recur (turn-guard-90-deg guard))
        (out-of-bounds? patrol-map next-pos) (assoc patrol-map :guard guard)
        :else                                (recur (update guard :path conj next-pos))))))



;; Solve part one
(time
  (-> input
      (input->patrol-map)
      (walk-guard-until-obstacle-or-out-of-bounds)
      (:guard)
      (:path)
      (distinct)
      (count)))

;; ## Part 2

;; We can attempt to solve part two by adding loop detection to our guard walking function.
;; We start keeping track of every place our guard has ever been (and the direction she is going)
;; then, before occupying a space, we check if we've been in that spot before walking that direction
;; and if so, we have a loop.

(defn walk-guard-until-obstacle-or-out-of-bounds-with-loop-detection
  "Walk the guard in the patrol map until it hits an obstacle or goes out of bounds"
  [{:keys [guard obstacles] :as patrol-map}]
  (loop [{:keys [direction path] :as guard} guard
         loop-history                       #{}]
    (let [guard-pos        (last path)
          loop-detected?   (loop-history [direction guard-pos])
          new-loop-history (conj loop-history [direction guard-pos])
          next-pos         (walk-in-direction guard-pos direction)]
      (cond
        loop-detected?                       :loop
        (obstacles next-pos)                 (recur (turn-guard-90-deg guard) new-loop-history)
        (out-of-bounds? patrol-map next-pos) (assoc patrol-map :guard guard)
        :else                                (recur (update guard :path conj next-pos)
                                                    new-loop-history)))))


;; This function ended up being able to solve the problem. There's probably something smarter that
;; could be done - but sometimes engineering is about knowing what's good enough.
;;
;;A few key performance enhancements / insights
;; 1) Use Clojure's `pmap` function to evaluate candidate positions in parallel.
;; 2) Use the first traversal to reduce the number of squares that we have to check. If the guard
;;    never touched that square then putting an obstacle there won't do anything
(defn find-loop-positions
  [{:keys [size obstacles guard] :as patrol-map}]
  (let [on-guard-path? (-> patrol-map
                           (walk-guard-until-obstacle-or-out-of-bounds)
                           :guard
                           (:path)
                           (set))]
    (->> (for [x     (range 0 (first size))
               y     (range 0 (second size))
               :let  [pos [x y]]
               :when (and (not (obstacles pos))
                          (not= (first (:path guard))
                                pos)
                          (on-guard-path? pos))]
           pos)
         (pmap (juxt identity #(walk-guard-until-obstacle-or-out-of-bounds-with-loop-detection
                                 (update patrol-map :obstacles conj %))))
         (filter (comp #{:loop} second))
         (map first))))

;; Solve part two - it's slow so to save Github's servers we will just keep it in a comment block
(comment
  (->> input
       (input->patrol-map)
       (find-loop-positions)))

(defn visualize-map
  [{:keys [obstacles guard size]} & {:keys [loop-obstacles]}]
  (let [pos->path-cnt  (frequencies (:path guard))
        loop-obstacles (set loop-obstacles)]
    (into
      [:div {:class ["grid" "bg-gray-50"]
             :style {:width                 "600px"
                     :height                "600px"
                     :grid-template-columns (format "repeat(%d, 1fr)" (first size))}}]
      (for [y    (range 0 (second size))
            x    (range 0 (first size))
            :let [pos [x y]]]
        [:div {:class ["border-2"
                       (when (= (first (:path guard)) pos)
                         "border-dashed border-blue border-2")
                       (when (loop-obstacles pos)
                         "border-dashed border-red-500 border-2")
                       (cond
                         (obstacles pos)           "bg-red-300"
                         (= 1 (pos->path-cnt pos)) "bg-blue-200"
                         (= 2 (pos->path-cnt pos)) "bg-blue-300"
                         :else                     nil)]}]))))

;; Visualize the map:
;; - **Blue Dashed**:  Starting position
;; - **Blue**: guard path, darker if occupied more than once
;; - **Red**: obstacle
;; - **Red Dashed** - Loop obstacle position
(let [patrol-map     (-> (aoc/example 6 :lines true)
                         (input->patrol-map))
      loop-positions (find-loop-positions patrol-map)
      finished-path  (walk-guard-until-obstacle-or-out-of-bounds patrol-map)]
  (clerk/html
    (visualize-map
      finished-path
      {:loop-obstacles loop-positions})))
