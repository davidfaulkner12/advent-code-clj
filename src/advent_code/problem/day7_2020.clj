(ns advent-code.problem.day7-2020
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

(defmethod ifaces/run-problem ["day7-2020" "1"] [x y z]
  (count (walk-tree "shiny gold" (merge-all-rules (map parse-rule-invert (split-lines z))))))

(defn parse-rule [[_ outer innerstr]]
  (let [split-inners (split-inners innerstr)
        inners (map #(vector (nth % 2) (Integer/parseInt (nth % 1))) split-inners)]
    {outer inners}))

; (apply concat ...) is the Clojure idiom for "flatten by 1 level"
(defn multiple-bags [count baglist]
  (apply concat (repeat count baglist)))

; I am aware this is a comically poor implementation of this algorithm, but it's
; surprisingly fast and I was very impatient by the end.
(defn walk-tree-count [start rules]
  (loop [count 0 to-process (rules start)]
    (if (empty? to-process)
      count
      (let [[cur-node cur-count] (first to-process)
            next-steps (rules cur-node)]
        (recur (+ count cur-count)
               (concat (rest to-process)
                       (multiple-bags cur-count next-steps)))))))

(defmethod ifaces/run-problem ["day7-2020" "2"] [x y z]
  (walk-tree-count "shiny gold" (merge-all-rules (map parse-rule (split-lines z)))))
