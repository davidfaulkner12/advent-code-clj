(ns advent-code.problem.day2-2020-1-test
  (:require [clojure.test :refer :all]
            [advent-code.problem.day2-2020-1 :refer :all]))

(deftest examples
  (are [x y] (= (check-triplet-count x) y)
    ["1-3" "a:" "abcde"] true
    ["1-3" "b:" "cdefg"] false
    ["2-9" "c:" "ccccccccc"] true))
