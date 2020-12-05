(ns advent-code.problem.day3-2020-test
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

(deftest calculate-product
  (is (= (*
          (calculate-trees (make-slope 1 1) parsed-map)
          (calculate-trees (make-slope 3 1) parsed-map)
          (calculate-trees (make-slope 5 1) parsed-map)
          (calculate-trees (make-slope 7 1) parsed-map)
          (calculate-trees (make-slope 1 2) parsed-map))
         336)))
