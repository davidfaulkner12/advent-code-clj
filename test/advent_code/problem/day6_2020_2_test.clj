(ns advent-code.problem.day5-2020-2-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.problem.day6-2020-1-test :refer :all]
            [advent-code.problem.day6-2020-1 :refer :all]))

(deftest example-test-2
  (is (= 6
         (apply + (map count (map parse-line-2 (split-data example)))))))

(deftest actual-problem-2
  (is (= 3254
         (ifaces/run-problem "day6-2020-2" (slurp "resources/2020-day6.input")))))
