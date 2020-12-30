(ns advent-code.problem.day24-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn parse-line [line]
  (re-seq #"[ns]?[ew]" line))

(defn parse-data [raw]
  (map parse-line (dh/split-lines raw)))

; Shamelessly stolen from https://www.redblobgames.com/grids/hexagons/
(def dir-vectors
  {"e" [1 -1 0]
   "se" [0 -1 1]
   "sw" [-1 0 1]
   "w" [-1 1 0]
   "nw" [0 1 -1]
   "ne" [1 0 -1]})


(defn walk-hex-map [dirs]
  (reduce (fn [cur dir]
            ;(println cur dir)
            (mapv + cur (dir-vectors dir)))
          [0 0 0]
          dirs))

(defn flip-tiles [work-list]
  (reduce (fn [black-set dirs]
            (let [flip-coordinate (walk-hex-map dirs)]
              ;(println black-set flip-coordinate)
              (if (contains? black-set flip-coordinate)
                (disj black-set flip-coordinate)
                (conj black-set flip-coordinate))))
          #{}
          work-list))

(defmethod ifaces/run-problem ["day24-2020" "1"] [x y z]
  (let [data (parse-data z)]
    (count (flip-tiles data))))

(defn adj-hex-raw [hex-coordinates]
  (map #(mapv + hex-coordinates %) (vals dir-vectors)))

(def adj-hex (memoize adj-hex-raw))

(defn adj-hex-set [black-set]
  (into black-set (mapcat adj-hex black-set)))

(defn count-black-adjacent [black-set coordinates]
  (let [adj-set (set (adj-hex coordinates))]
    (count (set/intersection black-set adj-set))))

(defn next-day [black-set]
  (let [all-possible-cells (adj-hex-set black-set)]
    (reduce (fn [next-black-set coords]
              (let [black-count (count-black-adjacent black-set coords)]
                (if (contains? black-set coords)
                    (if (or (= black-count 0) (> black-count 2))
                        next-black-set
                        (conj next-black-set coords))
                    (if (= black-count 2)
                        (conj next-black-set coords)
                        next-black-set))))
            #{}
            all-possible-cells)))


(defmethod ifaces/run-problem ["day24-2020" "2"] [x y z]
  (let [data (parse-data z)
        starting-set (flip-tiles data)]
    (count (nth (iterate next-day starting-set) 100))))
