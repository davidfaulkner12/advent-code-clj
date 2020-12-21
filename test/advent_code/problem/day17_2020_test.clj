(ns advent-code.problem.day17-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]
            [advent-code.problem.day17-2020 :refer :all]))

(def raw-example-1 ".#.
..#
###
")

(deftest parse-raw-example-1
  (is (= #{[-1 1 0] [0 -1 0] [0 1 0] [1 0 0] [1 1 0]}
         (parse-map raw-example-1 -1 -1 0))))

(def example-1 (parse-map raw-example-1 -1 -1 0))

(deftest count-adjacency-for-point
  (are [x y] (= y (count-adj-active-for-point example-1 x))
    [0 0 0] 5
    [-1 -1 -1] 1
    [3034 -12389312 3423840981] 0))

(def raw-example-2-neg1 "#..
..#
.#.
")

(def raw-example-2-0 "#.#
.##
.#.
")

(def raw-example-2-pos1 "#..
..#
.#.
")

(def example-2
   (set/union (parse-map raw-example-2-neg1 -1 0 -1)
              (parse-map raw-example-2-0 -1 0 0)
              (parse-map raw-example-2-pos1 -1 0 1)))

(deftest test-count-cycles
  (are [x y] (= y (count-cycles example-1 x))
    0 5
    1 11
    2 21
    3 38
    6 112))

(deftest actual-problem-1
  (is (= 4
         (ifaces/run-problem "day17-2020" "1" (slurp "resources/2020-day17.input")))))
