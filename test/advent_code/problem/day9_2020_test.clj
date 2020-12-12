(ns advent-code.problem.day9-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day9-2020 :refer :all]))


(def example (dh/to-edn-vec "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
"))

(deftest actual-problem-1
  (is (= 756008079
         (ifaces/run-problem "day9-2020" "1" (slurp "resources/2020-day9.input")))))

(deftest actual-problem-2
  (is (= 93727241
         (ifaces/run-problem "day9-2020" "2" (slurp "resources/2020-day9.input")))))
