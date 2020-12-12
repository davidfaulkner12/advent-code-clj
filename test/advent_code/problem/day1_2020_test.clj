(ns advent-code.problem.day1-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.problem.day1-2020 :refer :all]))

(deftest example-1
  (is (= 514579
         (multiply-match-for-2020 2 [1721 979 366 299 675 1456]))))

(deftest actual-problem-1
  (is (= 381699
         (ifaces/run-problem "day1-2020" "1" (slurp "resources/2020-day1.input")))))

(deftest example-2
  (is (= 241861950
         (multiply-match-for-2020 3 [1721 979 366 299 675 1456]))))

(deftest actual-problem-2
  (is (= 111605670
         (ifaces/run-problem "day1-2020" "2" (slurp "resources/2020-day1.input")))))
