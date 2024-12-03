(ns aoc.day-3-test
  (:require [aoc.day-3 :as sut]
            [aoc]
            [clojure.test :refer [deftest testing is]]))

(def example-input
  "Some example input"
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(deftest parse-part-one-instructions-test
  (is (= [[:mul 2 4]
          [:mul 5 5]
          [:mul 11 8]
          [:mul 8 5]]
         (sut/parse-part-one-instructions example-input))))

(def example-input-two
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(deftest parse-part-two-instructions-test
  (is (= '([:mul 2 4] [:dont] [:mul 5 5] [:mul 11 8] [:do] [:mul 8 5])
         (sut/parse-part-two-instructions example-input-two))))
