(ns advent-code.problem.day24-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day24-2020 :refer :all]))

(def raw-example "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew
")

(deftest samples
  (are [x y] (= (walk-hex-map x) y)
    ["e" "e" "w" "w"] [0 0 0]
    ["e" "se" "w"] [0 -1 1]
    ["nw" "w" "sw" "e" "e"] [0 0 0]))

(deftest basic-flip-tests
  (are [x y] (= (flip-tiles x) y)
    [["e" "w"]] #{[0 0 0]}
    [["e" "w"] ["w" "e"]] #{}))

(deftest test-example-problem
  (is (= 10
         (count (flip-tiles (parse-data raw-example))))))

(deftest test-actual-problem
  (is (= 346
         (ifaces/run-problem "day24-2020" "1" (slurp "resources/2020-day24.input")))))

(def ex-set (flip-tiles (parse-data raw-example)))

(deftest given-examples-day-iteration
  (are [x y] (= (count (nth (iterate next-day ex-set) x)) y)
    1 15
    2 12
    10 37
    100 2208))

(deftest test-actual-problem
  (is (= 3802
         (ifaces/run-problem "day24-2020" "2" (slurp "resources/2020-day24.input")))))
