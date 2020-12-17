(ns advent-code.problem.day11-2020
  (:require [clojure.set :as set]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn parse-data [raw]
  (mapv vec (dh/split-lines raw)))

(defn count-occupied-seats [floormap]
    (count (filter #(= \# %) (flatten floormap))))

(def adjacency-moves
  (for [x [-1 0 1]
        y [-1 0 1]
        :when (or (not= x 0) (not= y 0))]
    [x y]))

(defn make-adjacency-function [max-x max-y]
  (fn [rowcol]
    (filterv
      (fn [[row col]] (and (< -1 row max-x)
                           (< -1 col max-y)))
      (mapv #(mapv + rowcol %) adjacency-moves))))

(def make-adjacency-function-memo (memoize make-adjacency-function))

(defn count-adjacent-occupied [floormap adjfn rowcol]
  (count (filter #(= \# %) (map #(get-in floormap %) (adjfn rowcol)))))

(defn next-value-adjacent [floormap rowcol]
  (let [max-x (count floormap)
        max-y (count (first floormap))
        adjfn (make-adjacency-function-memo max-x max-y)
        cur-seat (get-in floormap rowcol)]
    (if (= \. cur-seat)
        \.
        (let [adj-count (count-adjacent-occupied floormap adjfn rowcol)]
          (cond (and (= 0 adj-count) (= \L cur-seat)) \#
                (and (>= adj-count 4) (= \# cur-seat)) \L
                :else cur-seat)))))

(defn make-next-map [floormap next-value-fn]
  (let [max-x (count floormap)
        max-y (count (first floormap))]
    (mapv vec
      (partition max-y
        (for [x (range 0 max-x)
              y (range 0 max-y)]
          (next-value-fn floormap [x y]))))))

(defn stable-map [floormap next-value-fn]
  (loop [current-map floormap]
    (let [next-map (make-next-map current-map next-value-fn)]
      (if (= next-map current-map)
          current-map
          (recur next-map)))))

(defmethod ifaces/run-problem ["day11-2020" "1"] [x y z]
  (count-occupied-seats (stable-map (parse-data z) next-value-adjacent)))

(def visible-fns
  (map
    (fn [[dx dy]]
      (fn [[x y]]
        [(+ x dx) (+ y dy)]))
    adjacency-moves))

(defn make-boundary [max-x max-y]
  (fn [[row col]] (and (< -1 row max-x)
                       (< -1 col max-y))))

(def make-boundary-memo (memoize make-boundary))

(defn find-occupied [floormap slope-fn rowcol]
  (let [max-x (count floormap)
        max-y (count (first floormap))
        boundary-fn (make-boundary-memo max-x max-y)
        ; Why rest? The first value of iterate is our starting seat
        possible-seats (take-while boundary-fn (rest (iterate slope-fn rowcol)))
        visible-seats (map #(get-in floormap %) possible-seats)]
    (if (= \# (first (drop-while #(= \. %) visible-seats)))
        1
        0)))

(defn count-visible-occupied [floormap rowcol]
  (apply + (map #(find-occupied floormap % rowcol) visible-fns)))

(defn next-value-visible [floormap rowcol]
  (let [cur-seat (get-in floormap rowcol)]
    (if (= \. cur-seat)
        \.
        (let [vis-count (count-visible-occupied floormap rowcol)]
          (cond (and (= 0 vis-count) (= \L cur-seat)) \#
                (and (>= vis-count 5) (= \# cur-seat)) \L
                :else cur-seat)))))


(defmethod ifaces/run-problem ["day11-2020" "2"] [x y z]
  (count-occupied-seats (stable-map (parse-data z) next-value-visible)))
