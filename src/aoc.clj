(ns aoc
  "Namespace utility for common functions for solving AOC"
  (:require [clojure.string :as str]))

(def ^:private -process-input-docs
  "Optionally takes the following arguments:

  :lines - if true, process the input as a seq of lines
  :ints - if true, parse ints from the input")

(defn process-input
  {:doc
   (str "Utility function for transforming the raw input into a more usable form."
        -process-input-docs)}
  [raw-input & {:keys [lines ints]}]
  (let [parse-line (fn [in]
                     (as-> in in
                       (if ints
                         (map parse-long (re-seq #"\d+" in))
                         in)))]
    (cond->> raw-input
      lines       (str/split-lines)
      (not lines) (vector)
      true        (map parse-line)
      (not lines) (first))))

(defn input
  {:doc
   (str
     "Function to return the input for the given day.\n\n"
     -process-input-docs)
   :arglists
   (list (into '[day] (-> #'process-input meta :arglists first rest)))}
  [day & {:as opts}]
  (-> (slurp (str "resources/day_" day "/input.txt"))
      (process-input opts)))

(defn example
  {:doc
   (str
     "Function to return the example for the given day.\n\n"
     -process-input-docs)
   :arglists
   (list (into '[day] (-> #'process-input meta :arglists first rest)))}
  [day & {:as opts}]
  (-> (slurp (str "resources/day_" day "/example.txt"))
      (process-input opts)))

(defn transpose
  "Transpose a matrix"
  [m]
  (reduce
    (fn [acc row]
      (mapv conj acc row))
    (vec (repeat (count (first m)) []))
    m))
