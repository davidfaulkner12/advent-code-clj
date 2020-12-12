(ns advent-code.interfaces-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :refer :all]))

(defmethod run-problem ["always-bob" "1"] [x y z] "bob")
(defmethod run-problem ["pass-through" "1"] [x y z] z)

(deftest always-bob
  (is (= "bob"
         (run-problem "always-bob" "1" "steve"))))

(deftest pass-through
  (is (= "steve"
         (run-problem "pass-through" "1" "steve"))))
