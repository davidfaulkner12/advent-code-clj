(ns advent-code.problem.day22-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day22-2020 :refer :all]))

(def raw-example "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10
")

(def example (parse-data raw-example))

(deftest test-example-on-real-problem
  (is (= 306
         (ifaces/run-problem "day22-2020" "1" raw-example))))

(deftest test-actual-problem
  (is (= 31314
         (ifaces/run-problem "day22-2020" "1" (slurp "resources/2020-day22.input")))))

(deftest test-example-on-real-problem
  (is (= 291
         (ifaces/run-problem "day22-2020" "2" raw-example))))

(deftest test-recursion-check
  (is (= 105
         (ifaces/run-problem "day22-2020" "2" "Player 1:
43
19

Player 2:
2
29
14
"))))

(deftest test-example-on-real-problem
  (is (= 32760
         (ifaces/run-problem "day22-2020" "2" (slurp "resources/2020-day22.input")))))
