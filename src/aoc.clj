(ns aoc
  "Namespace utility for common functions for solving AOC"
  (:require [clojure.string :as str]
            [hato.client :as http]
            [clojure.java.io :as io]))

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

(def ^:private -AOC-SESSION-ENV-VAR
  "The name of the environment variable that should contain the Advent of Code session ID."
  "AOC_SESSION_ID")

(defn- -fetch-remote-input
  "Utility function to fetch the input for the given day from the Acvent of Code website."
  ([day]
   (-fetch-remote-input day (.getYear (java.time.LocalDate/now))))
  ([day year]
   (let [session-id (System/getenv -AOC-SESSION-ENV-VAR)]
     (assert session-id (format "%s environment variable not set" -AOC-SESSION-ENV-VAR))
     (-> (http/get (str "https://adventofcode.com/" year "/day/" day "/input")
                   {:headers {"Cookie" (str "session=" session-id)}})
         :body))))

(defn input
  {:doc
   (str
     "Function to return the input for the given day.\n\n"
     -process-input-docs)
   :arglists
   (list (into '[day] (-> #'process-input meta :arglists first rest)))}
  [day & {:as opts}]
  (let [file-path (str "resources/day_" day "/input.txt")
        f         (io/file file-path)]
    (-> (if (.exists f)
          (slurp f)
          (let [_    (when-not (System/getenv "AOC_SESSION_ID")
                       (throw (ex-info
                                (str/join
                                  "\n"
                                  ["Attempted to open missing puzzle input."
                                   (format "Please either set %s or provide the input file in resources/day_%d/input.txt"
                                           -AOC-SESSION-ENV-VAR
                                           day)])
                                {:day  day
                                 :path file-path})))
                _    (println "Fetching data for day" day)
                data (-fetch-remote-input day)]
            (spit f data)
            data))
        (process-input opts))))

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
