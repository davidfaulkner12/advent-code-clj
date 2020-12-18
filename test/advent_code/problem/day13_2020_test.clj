(ns advent-code.problem.day13-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day13-2020 :refer :all]))

(def raw-example "939
7,13,x,x,59,x,31,19
")

(def example-1 (parse-data raw-example))

(deftest test-example-1
  (is (= [944 59]
         (next-bus example-1))))

(deftest test-example-problem
  (is (= 295
         (ifaces/run-problem "day13-2020" "1" raw-example))))

(deftest actual-problem
  (is (= 2545
         (ifaces/run-problem "day13-2020" "1" (slurp "resources/2020-day13.input")))))

(def example-2-1 (parse-data-2 raw-example))

(deftest simple-prob2-test
  (is (not (valid-schedule? 0  example-2-1))))

(deftest given-prob2-test
  (is (valid-schedule? 1068781 example-2-1)))

(deftest test-chinese-remainder
  (is (= 39
         (chinese-remainder {3 0, 4 3, 5 4}))))

(deftest example-given-1
  (is (= 1068781
         (chinese-remainder (invert-schedule example-2-1)))))

(deftest example-given-2
  (is (= 3417
          (chinese-remainder (invert-schedule [17,1,13,19])))))

(deftest example-given-3
  (is (= 754018
          (chinese-remainder (invert-schedule [67,7,59,61])))))

(deftest example-given-4
  (is (= 779210
          (chinese-remainder (invert-schedule [67,1,7,59,61])))))

(deftest example-given-5
  (is (= 1261476
          (chinese-remainder (invert-schedule [67,7,1,59,61])))))

(deftest example-given-6
  (is (= 1202161486
         (chinese-remainder (invert-schedule [1789,37,47,1889])))))

(deftest actual-problem-2
  (is (= 266204454441577
         (ifaces/run-problem "day13-2020" "2" (slurp "resources/2020-day13.input")))))
