(ns advent-code.problem.day2-2020-2-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.problem.day2-2020-1 :refer :all]
            [advent-code.problem.day2-2020-2 :refer :all]))

(deftest xor-test
  (are [x y z] (= (xor x y) z)
    false false false
    true false true
    false true true
    true true false))

(deftest examples
  (are [x y] (= ((check-line make-position-filter) x) y)
    [1 3 \a "abcde"] true
    [1 3 \b "cdefg"] false
    [2 9 \c "ccccccccc"] false
    [1 3 \a "cbade"] true)) ; Not an example but I screwed this up at first

(deftest actual-problem
  (is (= 303
         (ifaces/run-problem "day2-2020-2" (slurp "resources/2020-day2.input")))))
