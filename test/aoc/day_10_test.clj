(ns aoc.day-10-test
  (:require [aoc.day-10 :as sut]
            [aoc]
            [clojure.test :refer [deftest testing is]]))

(def example-input
  (aoc/example 10 {:lines true :digits true}))

(def trail-map
  (sut/input->trail-map example-input))


(deftest find-trailhead-paths
  (is (= '((:end))
         (sut/find-trailhead-paths trail-map [2 0])))
  (is (= '(([2 0] :end))
         (sut/find-trailhead-paths trail-map [2 1])))

  (is (every? #(= 11 (count %)) (sut/find-trailhead-paths trail-map))))


(deftest find-trailheads-with-score-test
  (let [pos->score (sut/find-trailheads-with-score trail-map)]
    (is (= 5 (pos->score [2 0])))
    (is (= 36 (reduce + (vals pos->score))))))


(deftest find-trailheads-with-ratings-test
  (let [pos->rating (sut/find-trailheads-with-rating trail-map)]
    (is (= 20 (pos->rating [2 0])))
    (is (= 81 (reduce + (vals pos->rating))))))
