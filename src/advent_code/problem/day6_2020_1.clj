(ns advent-code.problem.day6-2020-1
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]))
 
(defn parse-line [line]
  (disj (set (seq line)) \newline))

(defn parse-line-2 [line]
  (apply clojure.set/intersection (map parse-line (dh/split-lines line))))

(defmethod ifaces/run-problem "day6-2020-1" [x y]
  (apply + (map count (map parse-line (dh/split-paragraphs y)))))
