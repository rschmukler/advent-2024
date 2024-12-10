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

(defn split-q-with-pred
  "Return `[before x after]` on the given `q` that matches `pred`"
  [q pred]
  (loop [before (empty q)
         q      q]
    (if (empty? q)
      [before nil q]
      (let [x (first q)]
        (if (pred x)
          [before x (rest q)]
          (recur (conj before x) (rest q)))))))

(defn update-q-last
  "Update the last item in the provided `q`"
  [q f & args]
  (let [x (peek q)]
    (conj (pop q) (apply f x args))))


(defn step-compaction-q
  [fds mv-file-id]
  (let [[fds-before
         {:keys [free-space file-size] :as mv-file} fds-after] (split-q-with-pred
                                                                 fds
                                                                 #(= mv-file-id (:id %)))
        [fds-before-insert
         insert-file
         fds-after-insert]                                     (split-q-with-pred
                                                                 fds-before
                                                                 #(>= (:free-space %) file-size))]
    (if-not insert-file
      fds
      (let [fds-after-insert (if (seq fds-after-insert)
                               (update-q-last fds-after-insert
                                              update :free-space + free-space file-size)
                               fds-after-insert)
            mv-file          (if (seq fds-after-insert)
                               (assoc mv-file :free-space (- (:free-space insert-file) file-size))
                               (update mv-file :free-space + file-size))
            insert-file      (assoc insert-file :free-space 0)]
        (ft/ft-concat
          (conj fds-before-insert insert-file mv-file)
          (ft/ft-concat fds-after-insert fds-after))))))

(defn simulate-compaction-part-two-q
  "Given a list of `fds` return the fds after compaction"
  [fds]
  (->> (map :id fds)
       (reverse)
       (butlast)
       (reduce step-compaction-q (into (ft/counted-double-list) fds))))


(comment
  (-> (input->descriptors puzzle-input)
      (simulate-compaction-part-two-q)
      (initialize-disk)
      (compute-checksum)))
