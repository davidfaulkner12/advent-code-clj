(ns advent-code.problem.day7-2020-2-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day7-2020-1-test :refer :all]
            [advent-code.problem.day7-2020-1 :refer :all]
            [advent-code.problem.day7-2020-2 :refer :all]))

; Uses the same example as problem 1
(deftest example-test-problem-2
  (is (= 32
         (->> example
              (split-lines)
              (map parse-rule)
              (merge-all-rules)
              (walk-tree-count "shiny gold")))))

(def example2 "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
")

(deftest example-2-test-problem-2
  (is (= 126
         (->> example2
              (split-lines)
              (map parse-rule)
              (merge-all-rules)
              (walk-tree-count "shiny gold")))))

(deftest actual-problem-2
  (is (= 1038
         (ifaces/run-problem "day7-2020-2" (slurp "resources/2020-day7.input")))))
