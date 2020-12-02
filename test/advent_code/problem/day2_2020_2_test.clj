(ns advent-code.problem.day2-2020-2-test
  (:require [clojure.test :refer :all]
            [advent-code.problem.day2-2020-2 :refer :all]))

(deftest xor-test
  (are [x y z] (= (xor x y) z)
    false false false
    true false true
    false true true
    true true false))

(deftest examples
  (are [x y] (= (check-triplet-crazy x) y)
    ["1-3" "a:" "abcde"] true
    ["1-3" "b:" "cdefg"] false
    ["2-9" "c:" "ccccccccc"] false
    ["1-3" "a:" "cbade"] true)) ; Not an example but I screwed this up at first
