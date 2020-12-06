(ns advent-code.problem.day5-2020-2
  (:require [clojure.set :as set]
            [advent-code.interfaces :as ifaces]
            [advent-code.problem.day5-2020-1 :as parent]))

(defmethod ifaces/run-problem "day5-2020-2" [x y]
  (let [parsed-data (parent/parse-data y)
        occupied-rows-and-cols (set (map parent/rowcol-for-boarding-pass parsed-data))
        occupied-seat-ids (set (map parent/seat-id occupied-rows-and-cols))
        all-rows-and-cols (set (for [x (range 0 128) y (range 0 8)] [x y]))
        empty-rows-and-cols (set/difference all-rows-and-cols occupied-rows-and-cols)
        correct-seat (filter (fn [[row col]]
                              (and (contains? occupied-seat-ids (- (parent/seat-id [row col]) 1))
                                   (contains? occupied-seat-ids (+ (parent/seat-id [row col]) 1))))
                             empty-rows-and-cols)]
    (parent/seat-id (first correct-seat))))
