(ns advent-code.problem.day1-2020-1-test
  (:require [clojure.test :refer :all]
            [advent-code.problem.day1-2020-1 :refer :all]))

(deftest example
  (is (= 514579
         (multiply-match-for-2020 2 [1721 979 366 299 675 1456]))))
