(ns aoc.day-11-test
  (:require [aoc.day-11 :as sut]
            [clojure.test :refer [deftest testing is]]))

(deftest blink-at-stone-test
  (is (= '(1) (sut/blink-at-stone 0)))
  (is (= '(1 1) (sut/blink-at-stone 11)))
  (is (= '(12 0) (sut/blink-at-stone 1200)))
  (is (= '(2024) (sut/blink-at-stone 1))))

(deftest blink-at-stones-test
  (is (= '(253000 1 7)
         (sut/blink-at-stones (list 125 17))))

  (is (= '(253 0 2024 14168)
         (sut/blink-at-stones '(253000 1 7)))))

(deftest count-after-n-blinks-test
  (is (= 55312 (sut/count-after-n-blinks 25 (list 125 17)))))


(deftest blink-at-stones-fast-test
  (is (= {253000 1
          1 1
          7 1}
         (sut/blink-at-stones-fast {125 1
                                    17 1}))))


(deftest count-after-n-blinks-fast-test
  (is (= 55312 (sut/count-after-n-blinks-fast 25 (list 125 17)))))
