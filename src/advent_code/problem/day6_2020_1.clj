(ns advent-code.problem.day6-2020-1
  (:require [advent-code.interfaces :as ifaces]))

(defn split-data [raw]
  (clojure.string/split raw #"\n\n"))

(defn parse-line [line]
  (disj (set (seq  line)) \newline))

(defn parse-line-2 [line]
  (apply clojure.set/intersection (map parse-line (clojure.string/split line #"\n"))))

(defmethod ifaces/run-problem "day6-2020-1" [x y]
  (apply + (map count (map parse-line (split-data y)))))
