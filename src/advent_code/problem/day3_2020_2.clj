(ns advent-code.problem.day3-2020-2
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day3-2020-1 :as parent]))

(defmethod ifaces/run-problem "day3-2020-2" [x y]
  (let [parsed-data (parent/parse-data y)]
    (*
      (parent/calculate-trees (parent/make-slope 1 1) parsed-data)
      (parent/calculate-trees (parent/make-slope 3 1) parsed-data)
      (parent/calculate-trees (parent/make-slope 5 1) parsed-data)
      (parent/calculate-trees (parent/make-slope 7 1) parsed-data)
      (parent/calculate-trees (parent/make-slope 1 2) parsed-data))))
