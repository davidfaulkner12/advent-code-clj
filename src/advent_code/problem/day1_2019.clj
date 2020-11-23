(ns advent-code.problem.day1-2019
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]))

(defn mass->fuel [mass]
  (- (quot mass 3) 2))


(defmethod ifaces/run-problem "day1-2019" [x y]
  (apply + (map mass->fuel (dh/to-edn-vec y))))
