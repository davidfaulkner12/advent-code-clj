(ns advent-code.problem.day2-2020-2
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day2-2020-1 :as parent]))

(defn xor [a b]
  (or
    (and a (not b))
    (and (not a) b)))

(defn make-position-filter [start end]
  (fn [character password]
    (xor
      (= character (get password (- start 1)))
      (= character (get password (- end 1))))))

(defmethod ifaces/run-problem "day2-2020-2" [x y]
  (let [parsed-data (parent/parse-split-data (parent/split-data y))]
    (count (filter (parent/check-line make-position-filter) parsed-data))))
