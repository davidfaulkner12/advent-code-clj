(ns advent-code.problem.day9-2020
  (:require [clojure.set :as set]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn generate-all-sums [number-set]
  (set (for [x number-set
             y number-set
             :when (not= x y)]
         (+ x y))))

(defn find-valid-number [numbers preamble]
  (loop [previous-set (subvec numbers 0 preamble) rest-of-set (subvec numbers preamble) current-index 0]
    (let [sums (generate-all-sums previous-set)]
      (if (contains? sums (first rest-of-set))
          (recur (subvec numbers current-index (+ preamble current-index)) (subvec numbers (+ preamble current-index)) (+ current-index 1))
          (first rest-of-set)))))

(defmethod ifaces/run-problem ["day9-2020" "1"] [x y z]
  (find-valid-number (dh/to-edn-vec z) 25))

(defn sum-sequence [sequence target]
  (loop [length 1]
    (let [sub (subvec sequence 0 length)
          sum (apply + sub)]
      (cond (= sum target) sub
            (> sum target) nil
            :else (recur (+ length 1))))))

(defn find-contiguous-sum [numbers target]
  (loop [starts-at 0]
    (let [sub (subvec numbers starts-at)
          outcome (sum-sequence sub target)]
      (if outcome
          outcome
          (recur (+ starts-at 1))))))

(defmethod ifaces/run-problem ["day9-2020" "2"] [x y z]
  (let [outcome (find-contiguous-sum (dh/to-edn-vec z) 756008079)]
    (+ (apply max outcome) (apply min outcome))))
