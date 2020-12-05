(ns advent-code.problem.day3-2020-1-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.problem.day3-2020-1 :refer :all]))

(def example-map "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
")

(def parsed-map (parse-data example-map))

(deftest calculate-example-map
  (is (= (calculate-trees (make-slope 3 1) parsed-map)
         7)))

(deftest actual-problem
  (is (= 230
         (ifaces/run-problem "day3-2020-1" (slurp "resources/2020-day3.input")))))
