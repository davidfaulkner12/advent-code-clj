(ns advent-code.problem.day5-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.problem.day5-2020 :refer :all]))

(deftest examples
  (are [x y z] (and (= y x) (= z (seat-id x)))
    (rowcol-for-boarding-pass (seq "BFFFBBFRRR")) [70 7] 567
    (rowcol-for-boarding-pass (seq "FFFBBBFRRR")) [14 7] 119
    (rowcol-for-boarding-pass (seq "BBFFBBFRLL")) [102 4] 820))

(deftest actual-problem-1
  (is (= 864
         (ifaces/run-problem "day5-2020" "1" (slurp "resources/2020-day5.input")))))

(deftest actual-problem-2
  (is (= 739
         (ifaces/run-problem "day5-2020" "2" (slurp "resources/2020-day5.input")))))
