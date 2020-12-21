(ns advent-code.problem.day17-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn parse-map [raw-map start-x start-y z]
  (let [row-cols (mapv vec (dh/split-lines raw-map))]
    (set (for [x (range (count (first row-cols)))
               y (range (count row-cols))
               :when (= \# (get-in row-cols [y x]))]
           [(+ x start-x) (+ y start-y) z]))))

(defn create-adjacency-moves [c]
  (apply combo/cartesian-product (repeat c [-1 0 1])))

(def create-adjacency-moves-memo (memoize create-adjacency-moves))

(defn add-points [p1 p2]
  (mapv + p1 p2))

(defn create-adjacency-for-point [p]
  (map (partial add-points p) (create-adjacency-moves (count p))))

(defn all-adjacent-points [point-set]
  (set (apply concat (map create-adjacency-for-point point-set))))

(defn count-adj-active-for-point [active-point-set point]
  (let [adjacent-to-point (set (create-adjacency-for-point point))]
    (count (set/intersection adjacent-to-point (disj active-point-set point)))))

(defn count-all-adjs [active-point-set adj-set]
  (map (fn [p] [p (count-adj-active-for-point active-point-set p)]) adj-set))

(defn calc-next-map [active-point-set adj-counts]
  (set (map first
         (filter (fn [[p c]]
                   (or (and (contains? active-point-set p) (<= 2 c 3))
                       (= c 3)))
                 adj-counts))))

(defn next-map [active-points]
  (let [adj-count (count-all-adjs active-points (all-adjacent-points active-points))]
    (calc-next-map active-points adj-count)))

(defn count-cycles [active-points n]
  (let [map-iter (iterate next-map active-points)
        point-map (nth map-iter n)]
    (count point-map)))

(defmethod ifaces/run-problem ["day17-2020" "1"] [x y z]
  (let [point-map (parse-map z 0 0 0)]
    (count-cycles point-map 6)))

(defn parse-map-4d [raw-map start-x start-y z w]
  (let [row-cols (mapv vec (dh/split-lines raw-map))]
    (set (for [x (range (count (first row-cols)))
               y (range (count row-cols))
               :when (= \# (get-in row-cols [y x]))]
           [(+ x start-x) (+ y start-y) z w]))))

(defmethod ifaces/run-problem ["day17-2020" "2"] [x y z]
  (let [point-map (parse-map-4d z 0 0 0 0)]
    (count-cycles point-map 6)))
