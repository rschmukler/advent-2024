(ns aoc.day-10
  (:require [aoc]))

(def input
  (aoc/input 10 {:lines true :digits true}))

(defn- neighboring-points
  [[x y]]
  [[(dec x) y]
   [(inc x) y]
   [x (inc y)]
   [x (dec y)]])

(defn input->trail-map
  [input]
  (let [size [(count (first input)) (count input)]
        base-data
        {:size size
         :points
         (into
           {}
           (for [[y line]      (map-indexed vector input)
                 [x elevation] (map-indexed vector line)]
             [[x y] {:elevation elevation}]))}]
    (reduce
      (fn [acc pos]
        (let [elevation (get-in acc [:points pos :elevation])
              ups       (->> (neighboring-points pos)
                             (filter #(when-some [neighbor-el (get-in acc [:points % :elevation])]
                                        (= neighbor-el (inc elevation))))
                             (set))
              downs     (->> (neighboring-points pos)
                             (filter #(when-some [neighbor-el (get-in acc [:points % :elevation])]
                                        (= neighbor-el (dec elevation))))
                             (set))
              evens     (->> (neighboring-points pos)
                             (filter #(when-some [neighbor-el (get-in acc [:points % :elevation])]
                                        (= neighbor-el elevation)))
                             (set))]
          (-> acc
              (update-in [:points pos] merge {:ups ups :downs downs :evens evens :pos pos}))))
      base-data
      (for [x (range 0 (first size))
            y (range 0 (second size))]
        [x y]))))


(defn find-trailhead-paths
  ([trail-map]
   (for [start (->> (:points trail-map)
                    (vals)
                    (filter #(= 9 (:elevation %))))
         path  (find-trailhead-paths trail-map (:pos start))]
     (lazy-seq (cons (:pos start) path))))
  ([trail-map start]
   (println "Position" start "elevation:" (get-in trail-map [:points start :elevation]))
   (if (zero? (get-in trail-map [:points start :elevation]))
     (list (list :end))
     (for [down (get-in trail-map [:points start :downs])
           path (find-trailhead-paths trail-map down)]
       (lazy-seq (cons down path))))))


(defn find-trailheads-with-score
  [trail-map]
  (let [paths (find-trailhead-paths trail-map)]
    (-> (reduce
          (fn [acc path]
            (let [start (-> path butlast last)
                  end   (first path)]
              (update acc start (fnil conj #{}) end)))
          {}
          paths)
        (update-vals count))))


;; Solve part one

(->> input
     (input->trail-map)
     (find-trailheads-with-score)
     (vals)
     (reduce +))


;; Solve part two - Got to love when you had the right intuition on where the problem might go!

(defn find-trailheads-with-rating
  [trail-map]
  (let [paths (find-trailhead-paths trail-map)]
    (->> paths
         (map #(last (butlast %)))
         (frequencies))))

(->> input
     (input->trail-map)
     (find-trailheads-with-rating)
     (vals)
     (reduce +))
