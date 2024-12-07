(ns aoc.day-7-test
  (:require [aoc.day-7 :as sut]
            [aoc]
            [clojure.test :refer [deftest testing is]]))

(def example-input
  (aoc/example 7 {:lines true :ints true}))


(deftest solve-operation-sequence-test
  (is (= '((10 * 19)) (sut/solve-operation-sequence 190 [10 19] {:ops ['+ '*]})))
  (is (= '((81 + 40 * 27)
           (81 * 40 + 27)) (sut/solve-operation-sequence 3267 [81 40 27] {:ops ['+ '*]})))
  (is (= nil (sut/solve-operation-sequence 83 [17 5] {:ops ['* '*]}))))
