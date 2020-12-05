(ns advent-code.problem.day5-2020-1-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.problem.day5-2020-1 :refer :all]))

(deftest examples
  (are [x y z] (and (= y x) (= z (seat-id x)))
    (rowcol-for-boarding-pass (seq "BFFFBBFRRR")) [70 7] 567
    (rowcol-for-boarding-pass (seq "FFFBBBFRRR")) [14 7] 119
    (rowcol-for-boarding-pass (seq "BBFFBBFRLL")) [102 4] 820))

(deftest actual-problem
  (is (= 864
         (ifaces/run-problem "day5-2020-1" (slurp "resources/2020-day5.input")))))
