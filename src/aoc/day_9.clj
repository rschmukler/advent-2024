(ns aoc.day-9
  (:require [aoc]
            [clojure.data.finger-tree :as ft]))

(def puzzle-input
  (aoc/input 9))


;; Let's start by getting a sense of size
(count puzzle-input)

(defn input->descriptors
  "A function to convert input into a list of descriptors"
  [input]
  (for [[id [file-size free-space]] (->> (re-seq #"\d" input)
                                         (map parse-long)
                                         (partition 2 2 nil)
                                         (map-indexed vector))]
    {:id         id
     :file-size  file-size
     :free-space (or free-space 0)}))


(defn initialize-disk
  "Take file descriptions and initialize an array for the disk"
  [file-descriptors]
  (let [total-size (reduce + (mapcat (juxt :file-size :free-space) file-descriptors))
        disk       (int-array total-size -1)]
    (loop [ix                                                    0
           [{:keys [id file-size free-space] :as fd} & rest-fds] file-descriptors]
      (if-not fd
        disk
        (do (doseq [ix (range ix (+ ix file-size))]
              (aset-int disk ix id))
            (recur (+ ix file-size free-space)
                   rest-fds))))))


(defn run-compaction!
  "Run compaction (via mutation) on the disk"
  [disk]
  (loop [write-ix 0
         read-ix  (dec (count disk))]
    (cond
      (>= write-ix read-ix)          disk
      (= -1 (aget disk read-ix))     (recur write-ix (dec read-ix))
      (not= -1 (aget disk write-ix)) (recur (inc write-ix) read-ix)
      :else                          (do (aset-int disk write-ix (aget disk read-ix))
                                         (aset-int disk read-ix -1)
                                         (recur (inc write-ix) (dec read-ix))))))


(defn compute-checksum
  "Return the checksum for the provided disk"
  [disk]
  (->> disk
       (map-indexed vector)
       (remove (comp #{-1} second))
       (map #(apply * %))
       (reduce +)))


;; Solve part one

(->> puzzle-input
     (input->descriptors)
     (initialize-disk)
     (run-compaction!)
     (compute-checksum))


;; Solve part two

;; Naively we could just traverse the whole disk over and over but that will be a n^2
;; operation. Let's see...
(defn simulate-compaction-part-two
  "Given a list of `fds` return the fds after compaction"
  [fds]
  fds)
