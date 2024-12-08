(ns aoc.day-8-test
  (:require [aoc.day-8 :as sut]
            [aoc]
            [clojure.test :refer [deftest testing is are]]))

(def example-input
  (aoc/example 8 {:lines true}))

(def layout
  (sut/input->layout example-input))


(deftest antinodes-between-antennas-test
  (are [a b antinodes] (= antinodes (sut/antinodes-between-antennas (:size layout) a b))
    [3 1] [6 7] #{[4 3] [5 5]}
    [4 3] [8 4] #{[0 2]}))

(deftest find-antinodes-on-frequency-test
  (is (= #{[0 2]
           [3 1]
           [6 7]
           [2 6]}
         (sut/find-antinodes-on-frequency
           [10 10]
           #{[4 3] [8 4] [5 5]}
           sut/antinodes-between-antennas))))


(deftest find-antinodes-on-map-test
  (is (= 14 (count (sut/find-antinodes-on-map layout sut/antinodes-between-antennas)))))

(deftest antinodes-between-antennas-part-two-test
  (is (= 9
         (count
           (sut/find-antinodes-on-frequency
             [10 10]
             #{[0 0] [1 2] [3 1]}
             sut/antinodes-between-antennas-part-two))))

  (is (= 34
         (count (sut/find-antinodes-on-map layout sut/antinodes-between-antennas-part-two)))))
