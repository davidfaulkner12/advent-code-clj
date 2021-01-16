(ns advent-code.problem.day25-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day25-2020 :refer :all]))

(def example-card-public 5764801)
(def example-door-public 17807724)

(deftest test-parse-data
  (is (= [5764801 17807724]
         (parse-data "5764801\n17807724\n"))))


(deftest test-given-secret-loops
  (are [x y] (= (identify-secret-loop x) y)
    example-card-public 8
    example-door-public 11))

(deftest test-given-example
  (is (= {:card-secret-key 14897079
          :door-secret-key 14897079}
         (calc-secret-key example-door-public example-card-public))))

(deftest actual-problem
  (is (= 17032383
         (ifaces/run-problem "day25-2020" "1" (slurp "resources/2020-day25.input")))))
