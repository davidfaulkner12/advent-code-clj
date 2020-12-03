(ns advent-code.problem.day3-2020-1-test
  (:require [clojure.test :refer :all]
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

(deftest calculate-example-map
  (is (= (calculate-trees (make-slope 3 1) (parse-data example-map))
         7)))
