(ns aoc.day-6-test
  (:require [aoc.day-6 :as sut]
            [aoc]
            [clojure.test :refer [deftest is]]))

(def example-input
  (aoc/example 6 {:lines true}))

(def patrol-map
  (sut/input->patrol-map example-input))


(deftest walk-guard-until-obstacle-or-out-of-bounds-test
  (let [result (sut/walk-guard-until-obstacle-or-out-of-bounds patrol-map)]
    (is (= 41 (-> result :guard :path distinct count)))))


(deftest walk-guard-until-obstacle-or-out-of-bounds-with-loop-detection-test
  (is (= :loop
         (-> patrol-map
             (update :obstacles conj [3 6])
             (sut/walk-guard-until-obstacle-or-out-of-bounds-with-loop-detection)))))


(deftest find-loop-positions-test
  (is (= 6 (count (sut/find-loop-positions patrol-map)))))
