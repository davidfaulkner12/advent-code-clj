(ns advent-code.problem.day20-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day20-2020 :refer :all]))

(def raw-example "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
")

(def example (parse-data raw-example))

(def example-solution (solve-map example))

(deftest given-example
    (are [x y] (= (get-in example-solution [x :id] y))
      [0 0] 1951
      [0 1] 2729
      [0 2] 1951
      [1 0] 1489
      [1 1] 1427
      [1 2] 2311
      [2 0] 1171
      [2 1] 2473
      [2 2] 3079))

(deftest actual-problem-example
  (is (= 20899048083289
         (ifaces/run-problem "day20-2020" "1" raw-example))))

(deftest actual-problem-actual-data
  (is (= 23386616781851
         (ifaces/run-problem "day20-2020" "1" (slurp "resources/2020-day20.input")))))

(deftest test-rotate-matrix-90
  (are [x y] (= (rotate-matrix-90 x) y)
    [[1 4]
     [2 3]]
    [[4 3]
     [1 2]]))

(deftest test-flip-matrix-diagonal-1
  (are [x y] (= (flip-matrix-horizontal (rotate-matrix-90 x)) y)
    [[1 4]
     [2 3]]
    [[1 2]
     [4 3]]))

(deftest test-flip-matrix-diagonal-2
  (are [x y] (= (flip-matrix-vertical (rotate-matrix-90 x)) y)
    [[1 4]
     [2 3]]
    [[3 4]
     [2 1]]))

(def given-inside-map (mapv vec (dh/split-lines ".#.#..#.##...#.##..#####
###....#.#....#..#......
##.##.###.#.#..######...
###.#####...#.#####.#..#
##.#....#.##.####...#.##
...########.#....#####.#
....#..#...##..#.#.###..
.####...#..#.....#......
#..#.##..#..###.#.##....
#.####..#.####.#.#.###..
###.#.#...#.######.#..##
#.####....##..########.#
##..##.#...#...#.#.#.#..
...#..#..#.#.##..###.###
.#.#....#.##.#...###.##.
###.#...#..#.##.######..
.#.#.###.##.##.#..#.##..
.####.###.#...###.#..#.#
..#.#..#..#.#.#.####.###
#..####...#.#.#.###.###.
#####..#####...###....##
#.##..#..#...#..####...#
.#.###..##..##..####.##.
...###...##...#...#..###
")))

(deftest test-inside-map
  (is (= given-inside-map
         (flip-matrix-horizontal (inside-map example-solution)))))

(deftest basic-sea-monster
  (is (is-sea-monster? sea-monster [0 0])))

(deftest count-on-example
  (is (= 273
         (ifaces/run-problem "day20-2020" "2" raw-example))))

(deftest count-on-example
  (is (= 2376
         (ifaces/run-problem "day20-2020" "2" (slurp "resources/2020-day20.input")))))
