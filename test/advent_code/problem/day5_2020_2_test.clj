(ns advent-code.problem.day5-2020-2-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.problem.day5-2020-2 :refer :all]))

(deftest actual-problem
  (is (= 739
         (ifaces/run-problem "day5-2020-2" (slurp "resources/2020-day5.input")))))
