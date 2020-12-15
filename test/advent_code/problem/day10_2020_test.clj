(ns advent-code.problem.day10-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day10-2020 :refer :all]))


(def example-1 (dh/to-edn-vec "16
10
15
5
1
11
7
19
6
12
4
"))

(deftest example-1-test
  (is (= {3 5, 1 7}
         (difference-count example-1))))

(def example-2 (dh/to-edn-vec "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
"))

(deftest example-2-test
  (is (= {3 10, 1 22}
         (difference-count example-2))))

(deftest multiply-counts-ex2
  (is (= 220
         (multiply-counts (difference-count example-2)))))

(deftest actual-problem-1
  (is (= 2380
         (ifaces/run-problem "day10-2020" "1" (slurp "resources/2020-day10.input")))))

(deftest example-1-problem-2
  (is (= 8
         (apply * (map lookup-multiplier (sequential-ones (sequential-diffs example-1)))))))

(deftest example-2-problem-2
  (is (= 19208
         (apply * (map lookup-multiplier (sequential-ones (sequential-diffs example-2)))))))

(deftest actual-problem-2
  (is (= 48358655787008
         (ifaces/run-problem "day10-2020" "2" (slurp "resources/2020-day10.input")))))
