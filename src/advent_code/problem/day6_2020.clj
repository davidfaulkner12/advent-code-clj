(ns advent-code.problem.day6-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))


(defn parse-line [line]
  (disj (set (seq line)) \newline))

(defmethod ifaces/run-problem ["day6-2020" "1"] [x y z]
  (apply + (map count (map parse-line (dh/split-paragraphs z)))))

(defn parse-line-intersect [line]
  (apply set/intersection (map parse-line (dh/split-lines line))))

(defmethod ifaces/run-problem ["day6-2020" "2"] [x y z]
  (apply + (map count (map parse-line-intersect (dh/split-paragraphs z)))))
