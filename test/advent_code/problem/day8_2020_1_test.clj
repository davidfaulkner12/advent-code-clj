(ns advent-code.problem.day8-2020-1-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day8-2020-1 :refer :all]))

(def example "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
")

(deftest example-test
  (is (= [5 false]
         (run-instructions (make-instructions example)))))

(deftest actual-problem
  (is (= 1501
         (ifaces/run-problem "day8-2020-1" (slurp "resources/2020-day8.input")))))
