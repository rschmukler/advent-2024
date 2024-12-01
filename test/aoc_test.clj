(ns aoc-test
  (:require [aoc :as sut]
            [clojure.test :refer [deftest testing is]]))

(deftest process-input-test
  (testing ":lines"
    (is (= ["hello" "world"]
           (sut/process-input "hello\nworld" {:lines true}))))
  (testing ":ints"
    (is (= [1 2 3 4]
           (sut/process-input "1 2 3 4" {:ints true})))
    (is (= [1 2 3 4]
           (sut/process-input "1 2\n 3 4" {:ints true}))))
  (testing ":lines + :ints"
    (is (= [[3 4]
            [4 3]
            [2 5]]
           (sut/process-input
             "3  4\n4  3\n2  5"
             {:lines true :ints true})))))

(deftest transpose-test
  (is (= [[1 4]
          [2 5]
          [3 6]]
         (sut/transpose [[1 2 3]
                         [4 5 6]]))))
