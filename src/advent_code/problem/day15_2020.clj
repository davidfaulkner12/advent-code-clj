(ns advent-code.problem.day15-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn generate-spoken-set [starting-numbers]
  (let [c (count starting-numbers)
        processed-set (into {} (for [x (range (- c 1))] {(starting-numbers x) (+ x 1)}))]
    [(transient processed-set) (starting-numbers (- c 1)) c]))

(defn speak-next-number [[spoken-set last-number last-index]]
  (let [next-index (inc last-index)
        previous-index (get spoken-set last-number 0)
        next-number (if (= 0 previous-index) 0 (- last-index previous-index))]
    [(assoc! spoken-set last-number last-index) next-number next-index]))

(defn nth-spoken-number [starting-numbers n]
  (let [c (count starting-numbers)
        iter-n (- n c)]
    (second (nth (iterate speak-next-number (generate-spoken-set starting-numbers))
                 iter-n))))

(defmethod ifaces/run-problem ["day15-2020" "1"] [x y z]
  (nth-spoken-number (dh/to-edn-vec z) 2020))

(defmethod ifaces/run-problem ["day15-2020" "2"] [x y z]
  (nth-spoken-number (dh/to-edn-vec z) 30000000))
