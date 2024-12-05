(ns aoc.day-5-test
  (:require [aoc.day-5 :as sut]
            [aoc]
            [clojure.test :refer [deftest testing is]]))

(def example-input
  (aoc/example 5 {:lines true :ints true}))

(def example-puzzle
  (sut/parse-input example-input))


(deftest correct-order?-test
  (let [puzzle (sut/parse-input example-input)]
    (is (sut/correct-order? puzzle (first (:updates puzzle))))
    (is (sut/correct-order? puzzle (second (:updates puzzle))))
    (is (sut/correct-order? puzzle (nth (:updates puzzle) 2)))
    (is (not (sut/correct-order? puzzle (nth (:updates puzzle) 3))))
    (is (not (sut/correct-order? puzzle (nth (:updates puzzle) 4))))))


(deftest middle-page-test
  (is (= 61 (sut/middle-page '(75 47 61 53 29)))))


(deftest fix-order-test
  (is (= '(97 75 47 61 53)
         (sut/fix-order example-puzzle '(75 97 47 61 53)))))
