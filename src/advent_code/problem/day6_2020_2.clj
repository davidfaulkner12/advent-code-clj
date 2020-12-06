(ns advent-code.problem.day6-2020-2
  (:require [clojure.set :as set]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day6-2020-1 :as parent]))

(defn parse-line-intersect [line]
  (apply set/intersection (map parent/parse-line (dh/split-lines line))))

(defmethod ifaces/run-problem "day6-2020-2" [x y]
  (apply + (map count (map parse-line-intersect (dh/split-paragraphs y)))))
