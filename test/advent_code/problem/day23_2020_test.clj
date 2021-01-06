(ns advent-code.problem.day23-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day23-2020 :refer :all]))

(def raw-example "389125467")

(def example (parse-data raw-example))

(deftest test-example-problem
  (is (= "67384529"
         (ifaces/run-problem "day23-2020" "1" raw-example))))

(deftest test-actual-problem
  (is (= "98752463"
         (ifaces/run-problem "day23-2020" "1" (slurp "resources/2020-day23.input")))))

(deftest test-example-problem-map
  (is (= "67384529"
         (run-problem-1-map raw-example))))

(deftest test-actual-problem-map
  (is (= "98752463"
         (run-problem-1-map (slurp "resources/2020-day23.input")))))

(deftest test-example-problem-2
  (is (= 149245887792
         (ifaces/run-problem "day23-2020" "2" raw-example))))

(deftest test-actual-problem-2
  (is (= 2000455861
         (ifaces/run-problem "day23-2020" "2" (slurp "resources/2020-day23.input")))))
