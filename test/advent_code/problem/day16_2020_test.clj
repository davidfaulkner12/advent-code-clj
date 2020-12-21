(ns advent-code.problem.day16-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day16-2020 :refer :all]))

(def raw-example "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
")

(deftest test-example
  (is (= 71
         (let [[rules _ tickets] (parse-data raw-example)]
           (calculate-error-rate rules tickets)))))

(deftest test-actual-problem-1
  (is (= 28882
         (ifaces/run-problem "day16-2020" "1" (slurp "resources/2020-day16.input")))))

(deftest test-example-valid-ticket
  (is (= '([7 3 47])
         (let [[rules _ tickets] (parse-data raw-example)
               flat-rules (deconstruct-rules rules)]
           (filter (partial valid-for-rule? flat-rules) tickets)))))

(def raw-example-2 "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
")

(deftest test-example-2-column-values
  (is (= '([3 15 5] [9 1 14] [18 5 9])
         (let [[_ _ tickets] (parse-data raw-example-2)]
           (column-values tickets)))))

(deftest test-example-2-single-rule
  (is (let [[rules _ tickets] (parse-data raw-example-2)]
        (valid-for-rule? (rules "row") (first (column-values tickets))))))

(deftest test-example-2-all-rules
  (is (= {1 "class", 0 "row", 2 "seat"}
         (let [[rules _ tickets] (parse-data raw-example-2)]
           (find-rules-from-tickets rules tickets)))))

(deftest test-problem2-sample
  (is (= 1429779530273
         (ifaces/run-problem "day16-2020" "2" (slurp "resources/2020-day16.input")))))
