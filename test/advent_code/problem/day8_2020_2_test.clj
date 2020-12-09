(ns advent-code.problem.day8-2020-2-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day8-2020-1 :refer :all]
            [advent-code.problem.day8-2020-2 :refer :all]))

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

(deftest amended-xample-test
  (is (= [8 true]
         (run-instructions (make-instructions amended-example)))))

(deftest actual-problem
  (is (= 509
         (ifaces/run-problem "day8-2020-2" (slurp "resources/2020-day8.input")))))
