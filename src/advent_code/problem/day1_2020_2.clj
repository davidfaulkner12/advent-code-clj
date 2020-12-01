(ns advent-code.problem.day1-2020-2
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day1-2020-1 :as parent]))

(defmethod ifaces/run-problem "day1-2020-2" [x y]
  (parent/multiply-match-for-2020 3 (dh/to-edn-vec y)))
