(ns advent-code.problem.day7-2020-1
  (:require [clojure.set :as set]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]))

(defn split-lines [raw]
  (re-seq #"(\w+ \w+) bags contain (.*)\.\n" raw))

(defn split-inners [innerstr]
  (re-seq #"(\d+) (\w+ \w+) bag[s, ]?" innerstr))

(defn parse-rule-invert [[_ outer innerstr]]
  (let [split-inners (split-inners innerstr)
        ; Discard the number, we only want to match the contains
        inners (map #(nth % 2) split-inners)]
    (reduce (fn [ncoll inner]
              (assoc ncoll inner [outer]))
            {}
            inners)))

(defn merge-all-rules [rule-list]
  (apply (partial merge-with into) rule-list))

(defn walk-tree [start rules]
  (loop [processed #{} to-process #{start}]
    (if (empty? to-process)
      (disj processed start)
      (let [next-steps (set (mapcat rules to-process))
            next-to-process (set/difference next-steps processed)]
        (recur (set/union processed to-process) next-to-process)))))

(defmethod ifaces/run-problem "day7-2020-1" [x y]
  (count (walk-tree "shiny gold" (merge-all-rules (map parse-rule-invert (split-lines y))))))
