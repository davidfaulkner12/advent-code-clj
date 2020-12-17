(ns advent-code.problem.day11-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day11-2020 :refer :all]))


(def example-1 (parse-data "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
"))

(def example-1-step-2 (parse-data "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
"))

(def example-adjacency-function (make-adjacency-function-memo (count example-1) (count (first example-1))))

(deftest example-adjacency-function-test
  (is (= #{[0 1] [1 0] [1 1]}
         (set (example-adjacency-function [0 0])))))

(deftest example-adjacent-values-test
  (is (= 0
         (count-adjacent-occupied example-1 example-adjacency-function [0 0]))))

(deftest example-next-value
  (is (= \#
         (next-value-adjacent example-1 [0 0]))))

(deftest example-next-value-stable
  (is (= \.
         (next-value-adjacent example-1 [0 1]))))

(deftest next-map-test
  (is (= example-1-step-2
         (make-next-map example-1 next-value-adjacent))))

(deftest example-problem
  (is (= 37
         (count-occupied-seats (stable-map example-1 next-value-adjacent)))))

(deftest actual-problem-1
  (is (= 2166
         (ifaces/run-problem "day11-2020" "1" (slurp "resources/2020-day11.input")))))

(def raw-example-2-1 ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....")

(def raw-example-2-2 ".............
.L.L.#.#.#.#.
.............")

(def raw-example-2-3 ".##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##.")

(def example-2-1
  (parse-data raw-example-2-1))

(def example-2-2
  (parse-data raw-example-2-2))

(def example-2-3
  (parse-data raw-example-2-3))

(deftest example-2-1-basic
  (is (= 2
         (count-visible-occupied example-2-1 [0 0]))))

(deftest example-2-1-test
  (is (= 8
         (count-visible-occupied example-2-1 [4 3]))))

(deftest example-2-2-test-1
  (is (= 0
         (count-visible-occupied example-2-2 [1 1]))))

(deftest example-2-2-test-2
  (is (= 1
         (count-visible-occupied example-2-2 [1 3]))))

(deftest example-2-3-test
  (is (= 0
         (count-visible-occupied example-2-2 [4 3]))))

(def example-next-map-2 (parse-data "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
"))

(def example-next-map-3 (parse-data "#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#
"))

(deftest next-map-visible-test
  (is (= example-next-map-3
         (make-next-map example-next-map-2 next-value-visible))))

(deftest stable-example-1
  (is (= 26
         (count-occupied-seats (stable-map example-1 next-value-visible)))))

(deftest actual-problem-2
  (is (= 1955
         (ifaces/run-problem "day11-2020" "2" (slurp "resources/2020-day11.input")))))
