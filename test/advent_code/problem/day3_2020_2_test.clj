(ns advent-code.problem.day3-2020-2-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.problem.day3-2020-1 :refer :all]
            [advent-code.problem.day3-2020-2 :refer :all]
            [advent-code.problem.day3-2020-1-test :refer :all]))

(deftest calculate-product
  (is (= (*
          (calculate-trees (make-slope 1 1) parsed-map)
          (calculate-trees (make-slope 3 1) parsed-map)
          (calculate-trees (make-slope 5 1) parsed-map)
          (calculate-trees (make-slope 7 1) parsed-map)
          (calculate-trees (make-slope 1 2) parsed-map))
         336)))

(deftest actual-problem-2 ; Have to rename to refer all to the other test :-)
  (is (= 9533698720
         (ifaces/run-problem "day3-2020-2" (slurp "resources/2020-day3.input")))))
