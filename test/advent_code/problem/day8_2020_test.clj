(ns advent-code.problem.day8-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day8-2020 :refer :all]))

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

(deftest actual-problem-1
  (is (= 1501
         (ifaces/run-problem "day8-2020" "1" (slurp "resources/2020-day8.input")))))

(def amended-example "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
nop -4
acc +6
")

(deftest amended-example-test
  (is (= [8 true]
         (run-instructions (make-instructions amended-example)))))

(deftest actual-problem-2
  (is (= 509
         (ifaces/run-problem "day8-2020" "2" (slurp "resources/2020-day8.input")))))
