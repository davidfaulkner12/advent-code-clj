(ns advent-code.problem.day3-2020-1
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]))

(defn make-slope [right down]
  (fn [x y]
    [(+ x right) (+ y down)]))

(defn tree? [x y data]
  (let [row (nth data y)
        mod-col (rem x (count row))
        col (nth row mod-col)]
    (= col \#)))

(defn calculate-trees [slope map]
  (let [max-length (count map)]
    (loop [x 0 y 0 num-trees 0]
      (if (>= y max-length)
          num-trees
          (let [[new-x new-y] (slope x y)]
            (if (tree? x y map)
              (recur new-x new-y (+ num-trees 1))
              (recur new-x new-y num-trees)))))))

(defn parse-data [raw]
  (map seq (clojure.string/split raw #"\n")))

(defmethod ifaces/run-problem "day3-2020-1" [x y]
  (let [parsed-data (parse-data y)]
    (calculate-trees (make-slope 3 1) parsed-data)))
