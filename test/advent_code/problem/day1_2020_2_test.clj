(ns advent-code.problem.day1-2020-2-test
  (:require [clojure.test :refer :all]
            [advent-code.problem.day1-2020-1 :refer :all]))

(deftest example
  (is (= 241861950
         (multiply-match-for-2020 3 [1721 979 366 299 675 1456]))))
