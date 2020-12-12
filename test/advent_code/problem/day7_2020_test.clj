(ns advent-code.problem.day7-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day7-2020 :refer :all]))

(def example "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
")

(deftest example-test-1
  (is (= 4
         (->> example
              (split-lines)
              (map parse-rule-invert)
              (merge-all-rules)
              (walk-tree "shiny gold")
              (count)))))

(deftest actual-problem-1
  (is (= 257
         (ifaces/run-problem "day7-2020" "1" (slurp "resources/2020-day7.input")))))

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
         (ifaces/run-problem "day7-2020" "2" (slurp "resources/2020-day7.input")))))
