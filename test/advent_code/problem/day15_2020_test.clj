(ns advent-code.problem.day15-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day15-2020 :refer :all]))

(def example [0 3 6])

(deftest test-examples
  (are [x y] (= y (nth-spoken-number x 2020))
    [1,3,2] 1
    [2,1,3] 10
    [1,2,3] 27
    [2,3,1] 78
    [3,2,1] 438
    [3,1,2] 1836
    example 436))

(deftest actual-problem-1
  (is (= 662
         (ifaces/run-problem "day15-2020" "1" (slurp "resources/2020-day15.input")))))

; Too slow!!!
; (deftest test-examples
;   (are [x y] (= y (nth-spoken-number x 30000000))
;     [0,3,6] 175594
;     [0,3,6] 175594
;     [1,3,2] 2578
;     [2,1,3] 3544142
;     [1,2,3] 261214
;     [2,3,1] 6895259
;     [3,2,1] 18
;     [3,1,2] 362))

(deftest actual-problem-2
  (is (= 37312
         (ifaces/run-problem "day15-2020" "2" (slurp "resources/2020-day15.input")))))
