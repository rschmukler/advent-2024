(ns day-1-test
  (:require [day-1 :as sut]
            [aoc]
            [clojure.test :refer [deftest is]]))

(def example
  (sut/parse-puzzle-input (aoc/example 1)))

(deftest parse-puzzle-input-test
  (is (= [[3 4 2 1 3 3]
          [4 3 5 3 9 3]]
         example)))

(deftest compute-total-distance-test
  (is (= 11
         (sut/compute-total-distance example))))


(deftest compute-total-similarity-score-test
  (is (= 31
         (sut/compute-total-similarity-score example))))
