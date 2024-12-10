(ns aoc.day-9-test
  (:require [aoc.day-9 :as sut]
            [aoc]
            [clojure.test :refer [deftest testing is]]
            [clojure.data.finger-tree :as ft]))


(def example
  (aoc/example 9))

(def descriptors
  (sut/input->descriptors example))

(deftest initialize-disk-test
  (let [disk (sut/initialize-disk descriptors)]
    (is (= 42 (count disk))))

  (let [disk (sut/initialize-disk (sut/input->descriptors "12345"))]
    (is (= 15 (count disk)))
    (is (= [0 -1 -1 1 1 1 -1 -1 -1 -1 2 2 2 2 2]
           (vec disk)))))


(deftest run-compaction-test
  (is (= [0 2 2 1 1 1 2 2 2 -1 -1 -1 -1 -1 -1]
         (->> (sut/input->descriptors "12345")
              (sut/initialize-disk)
              (sut/run-compaction!)
              (into []))))
  (is (= [0 0 9 9 8 1 1 1 8 8 8 2 7 7 7 3 3 3 6 4 4 6 5 5 5 5 6 6
          -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1]
         (->> (sut/initialize-disk descriptors)
              (sut/run-compaction!)
              (into [])))))


(deftest run-checksum-test
  (is (= 1928 (-> descriptors
                  (sut/initialize-disk)
                  (sut/run-compaction!)
                  (sut/compute-checksum)))))

(defn str->q
  [s]
  (into
    (ft/counted-double-list)
    (for [[_ ids free-space] (re-seq #"(\d+)(\.*)" s)]
      {:id         (parse-long (str (first ids)))
       :file-size  (count ids)
       :free-space (count free-space)})))

(deftest step-compaction-q-test
  (let [fds (into (ft/counted-double-list) descriptors)]
    (is (= "00...111...2...333.44.5555.6666.777.888899"
           (sut/fds->str fds)))
    (is (= "0099.111...2...333.44.5555.6666.777.8888.."
           (-> fds
               (sut/step-compaction-q 9)
               (sut/fds->str))))
    (is (= "0099.1117772...333.44.5555.6666.....8888.."
           (-> fds
               (sut/step-compaction-q 9)
               (sut/step-compaction-q 8)
               (sut/step-compaction-q 7)
               (sut/fds->str))))
    (is (= "0099.111777244.333....5555.6666.....8888.."
           (-> fds
               (sut/step-compaction-q 9)
               (sut/step-compaction-q 8)
               (sut/step-compaction-q 7)
               (sut/step-compaction-q 6)
               (sut/step-compaction-q 5)
               (sut/step-compaction-q 4)
               (sut/fds->str))))
    (is (= "00992111777.44.333....5555.6666.....8888.."
           (-> fds
               (sut/step-compaction-q 9)
               (sut/step-compaction-q 8)
               (sut/step-compaction-q 7)
               (sut/step-compaction-q 6)
               (sut/step-compaction-q 5)
               (sut/step-compaction-q 4)
               (sut/step-compaction-q 3)
               (sut/step-compaction-q 2)
               (sut/step-compaction-q 1)
               (sut/fds->str))))

    (is (= "00111....."
           (-> (sut/step-compaction-q (str->q "00...111..") 1)
               (sut/fds->str))))))

(deftest simulate-compaction-part-two-q-test
  (is (= [{:id 0 :file-size 2 :free-space 0}
          {:id 9 :file-size 2 :free-space 0}
          {:id 2 :file-size 1 :free-space 0}
          {:id 1 :file-size 3 :free-space 0}
          {:id 7 :file-size 3 :free-space 1}
          {:id 4 :file-size 2 :free-space 1}
          {:id 3 :file-size 3 :free-space 4}
          {:id 5 :file-size 4 :free-space 1}
          {:id 6 :file-size 4 :free-space 5}
          {:id 8 :file-size 4 :free-space 2}]
         (sut/simulate-compaction-part-two descriptors)))

  (is (= 2858
         (->> descriptors
              (sut/simulate-compaction-part-two)
              (sut/initialize-disk)
              (sut/compute-checksum)))))
