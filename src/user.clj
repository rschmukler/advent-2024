(ns user
  (:require [nextjournal.clerk :as clerk]))

(clerk/serve! {:browse      true
               :paths       ["src/aoc/*"]
               :watch-paths ["src/aoc/"]})
