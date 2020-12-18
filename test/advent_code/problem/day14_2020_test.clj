(ns advent-code.problem.day14-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day14-2020 :refer :all]))

(def raw-example "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
")

(deftest test-36-bit-string
  (is (= "000000000000000000000000000000001011"
         (to-36bits 11))))

(deftest test-create-bitstring
  (is (= {29 \1, 34 \0}
         (create-bitmask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"))))

(deftest test-bit-set-with-mask
  (is (= 73
         (mask-bits 11 {29 \1, 34 \0}))))

(deftest run-example
  (is (= {8 64, 7 101}
         (run-program [["mask" "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"] [8 11] [7 101] [8 0]]))))

(deftest test-actual-example
 (is (= 165
        (apply + (vals (run-program (parse-data raw-example)))))))

(deftest actual-problem
  (is (= 2346881602152
         (ifaces/run-problem "day14-2020" "1" (slurp "resources/2020-day14.input")))))

(def raw-example-2 "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
")

(def example-2 (parse-data raw-example-2))

(deftest test-actual-example-2
 (is (= 208
        (ifaces/run-problem "day14-2020" "2" raw-example-2))))

(deftest test-actual-example-2
 (is (= 3885232834169
        (ifaces/run-problem "day14-2020" "2" (slurp "resources/2020-day14.input")))))
