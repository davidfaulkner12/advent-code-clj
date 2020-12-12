(ns advent-code.problem.day2-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.math.combinatorics :as combo]))

(defn make-count-filter [start end]
  (fn [character password]
    (let [count-chars (count (filter #(= character %) password))]
      (<= start count-chars end))))

(defn check-line [make-filter]
  (fn [[start end char password]]
    (let [testfn (make-filter start end)]
      (testfn char password))))

(defn parse-single-line [[_ startstr endstr charstr password]]
  [(Integer/parseInt startstr) (Integer/parseInt endstr) (first charstr) password])

(defn parse-split-data [split-data]
  (map parse-single-line split-data))

(defn split-data [raw]
  (re-seq #"(\d+)-(\d+) (\w): (\w+).*\n?" raw))

(defmethod ifaces/run-problem ["day2-2020" "1"] [x y z]
  (let [parsed-data (parse-split-data (split-data z))]
    (count (filter (check-line make-count-filter) parsed-data))))

(defn xor [a b]
  (or
    (and a (not b))
    (and (not a) b)))

(defn make-position-filter [start end]
  (fn [character password]
    (xor
      (= character (get password (- start 1)))
      (= character (get password (- end 1))))))

(defmethod ifaces/run-problem ["day2-2020" "2"] [x y z]
  (let [parsed-data (parse-split-data (split-data z))]
    (count (filter (check-line make-position-filter) parsed-data))))
