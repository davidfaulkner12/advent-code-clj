(ns advent-code.problem.day1-2019-test
  (:require [clojure.test :refer :all]
            [advent-code.problem.day1-2019 :refer :all]))

(deftest examples
  (are [x y] (= y (mass->fuel x))
    12 2
    14 2
    1969 654
    100756 33583))

(deftest sum-it-all
  (is (= 4
         (apply + (map mass->fuel [12 14])))))
