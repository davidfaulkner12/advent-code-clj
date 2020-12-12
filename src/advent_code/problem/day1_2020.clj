(ns advent-code.problem.day1-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.math.combinatorics :as combo]))

(defn multiply-match-for-2020 [number expenses]
  (apply *
    (first (filter #(= 2020 (apply + %))
                   (combo/combinations expenses number)))))

(defmethod ifaces/run-problem ["day1-2020" "1"] [x y z]
  (multiply-match-for-2020 2 (dh/to-edn-vec z)))

(defmethod ifaces/run-problem ["day1-2020" "2"] [x y z]
  (multiply-match-for-2020 3 (dh/to-edn-vec z)))
