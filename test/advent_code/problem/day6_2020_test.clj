(ns advent-code.problem.day6-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day6-2020 :refer :all]))

(def example "abc

a
b
c

ab
ac

a
a
a
a

b")

(deftest example-test
  (is (= 11
         (apply + (map count (map parse-line (dh/split-paragraphs example)))))))

(deftest actual-problem
  (is (= 6273
         (ifaces/run-problem "day6-2020" "1" (slurp "resources/2020-day6.input")))))

(deftest example-test-2
  (is (= 6
         (apply + (map count (map parse-line-intersect (dh/split-paragraphs example)))))))

(deftest actual-problem-2
  (is (= 3254
         (ifaces/run-problem "day6-2020" "2" (slurp "resources/2020-day6.input")))))
