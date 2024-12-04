(ns aoc.day-4-test
  (:require [aoc.day-4 :as sut]
            [aoc]
            [clojure.test :refer [deftest testing is]]))

(def example-input
  (aoc/example 4 :lines true))


(deftest find-word-paths-test
  (is (= 18
         (count (sut/find-paths example-input "XMAS")))))
