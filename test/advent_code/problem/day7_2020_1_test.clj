(ns advent-code.problem.day7-2020-1-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day7-2020-1 :refer :all]))

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

(deftest example-test
  (is (= 4
         (->> example
              (split-lines)
              (map parse-rule-invert)
              (merge-all-rules)
              (walk-tree "shiny gold")
              (count)))))

(deftest actual-problem
  (is (= 257
         (ifaces/run-problem "day7-2020-1" (slurp "resources/2020-day7.input")))))
