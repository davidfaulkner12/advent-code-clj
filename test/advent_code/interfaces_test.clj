(ns advent-code.interfaces-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :refer :all]))

(defmethod run-problem "always-bob" [x y] "bob")
(defmethod run-problem "pass-through" [x y] y)

(deftest always-bob
  (is (= "bob"
         (run-problem "always-bob" "steve"))))

(deftest pass-through
  (is (= "steve"
         (run-problem "pass-through" "steve"))))

(defmethod string->data "always-one" [x y] 1)
(defmethod string->data "pass-through" [x y] y)

(deftest s->d-one
  (is (= 1
         (string->data "always-one" 42))))

(deftest s->d->pass
  (is (= 42
         (string->data "pass-through" 42))))
