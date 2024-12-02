(ns aoc.day-2-test
  (:require [aoc.day-2 :as sut]
            [aoc]
            [clojure.test :refer [deftest are is]]))

(defonce example-input
  (aoc/example 2 {:lines true :ints true}))

(deftest safe?-test
  (is (sut/safe? [7 6 4 2 1]))
  (is (not (sut/safe? [1 2 7 8 9])))
  (is (not (sut/safe? [9 7 6 2 1])))
  (is (not (sut/safe? [1 3 2 4 5])))
  (is (not (sut/safe? [8 6 4 4 1])))
  (is (sut/safe? [1 3 6 7 9])))

(deftest safe-removing-one?-test
  (is (sut/safe-removing-one? [7 6 4 2 1]))
  (is (not (sut/safe-removing-one? [1 2 7 8 9])))
  (is (not (sut/safe-removing-one? [9 7 6 2 1])))
  (is (sut/safe-removing-one? [1 3 2 4 5]))
  (is (sut/safe-removing-one? [8 6 4 4 1]))
  (is (sut/safe-removing-one? [1 3 6 7 9])))
