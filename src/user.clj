(ns user
  (:require [nextjournal.clerk :as clerk]))

(clerk/serve! {:browse      true
               :port        7779
               :paths       ["src/aoc/*"]
               :watch-paths ["src/aoc/"]})

(comment
  (clerk/build!
    {:paths  ["src/aoc/*.clj"]
     :browse true}))
