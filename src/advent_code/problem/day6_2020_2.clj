(ns advent-code.problem.day6-2020-2
  (:require [clojure.set :as set]
            [advent-code.interfaces :as ifaces]
            [advent-code.problem.day6-2020-1 :as parent]))

(defn parse-line-intersect [line]
  (apply set/intersection (map parent/parse-line (clojure.string/split line #"\n"))))

(defmethod ifaces/run-problem "day6-2020-2" [x y]
  (apply + (map count (map parse-line-intersect (parent/split-data y)))))
