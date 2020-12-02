(ns advent-code.problem.day2-2020-2
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day2-2020-1 :as parent]))

(defn xor [a b]
  (or
    (and a (not b))
    (and (not a) b)))

(defn crazy-filter [rangestr]
    (let [[start end] (parent/rangestr-to-ints rangestr)]
      (fn [character password]
        (xor
          (= character (get password (- start 1)))
          (= character (get password (- end 1)))))))

(defn check-triplet-crazy [[rangestr characterplus password]]
  (let [testfn (crazy-filter rangestr)
        testchar (first characterplus)]
       (testfn testchar password)))

(defmethod ifaces/run-problem "day2-2020-2" [x y]
  (let [triplet-vec (parent/parse-data y)]
    (count (filter check-triplet-crazy triplet-vec))))
