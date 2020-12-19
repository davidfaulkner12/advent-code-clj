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
