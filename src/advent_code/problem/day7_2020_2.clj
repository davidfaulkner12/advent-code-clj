(ns advent-code.problem.day7-2020-2
  (:require [clojure.set :as set]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day7-2020-1 :as parent]))

(defn parse-rule [[_ outer innerstr]]
  (let [split-inners (parent/split-inners innerstr)
        inners (map #(vector (nth % 2) (Integer/parseInt (nth % 1))) split-inners)]
    {outer inners}))

; (apply concat ...) is the Clojure idiom for "flatten by 1 level"
(defn multiple-bags [count baglist]
  (apply concat (repeat count baglist)))

(defn walk-tree-count [start rules]
  (loop [count 0 to-process (rules start)]
    (if (empty? to-process)
      count
      (let [[cur-node cur-count] (first to-process)
            next-steps (rules cur-node)]
        (recur (+ count cur-count)
               (concat (rest to-process)
                       (multiple-bags cur-count next-steps)))))))

(defmethod ifaces/run-problem "day7-2020-2" [x y]
  (walk-tree-count "shiny gold" (parent/merge-all-rules (map parse-rule (parent/split-lines y)))))
