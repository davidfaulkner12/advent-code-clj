(ns advent-code.problem.day18-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day18-2020 :refer :all]))

(def raw-example "1 + 2 * 3 + 4 * 5 + 6")

(deftest test-execute-no-parens
  (is (= 71
         (execute-line (parse-line raw-example)))))

(def raw-example-2 "1 + (2 * 3) + (4 * (5 + 6))")

(deftest test-execute-parens
  (is (= 51
         (execute-line (parse-line raw-example-2)))))

(deftest actual-problem
  (is (= 1408133923393
         (ifaces/run-problem "day18-2020" "1" (slurp "resources/2020-day18.input")))))

(deftest example-problem-2
  (is (= 231
         (calculate-rpn (process-tokens (parse-line-2 raw-example))))))

(deftest test-example-problem-2-with-parens
  (is (= 51
         (calculate-rpn (process-tokens (parse-line-2 raw-example-2))))))

(deftest actual-problem-2
  (is (= 314455761823725
         (ifaces/run-problem "day18-2020" "2" (slurp "resources/2020-day18.input")))))
