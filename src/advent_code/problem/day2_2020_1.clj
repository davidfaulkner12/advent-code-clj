(ns advent-code.problem.day2-2020-1
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.math.combinatorics :as combo]))

(defn rangestr-to-ints [rangestr]
  (let [[startc endc] (clojure.string/split rangestr #"\-")]
    [(Integer/parseInt startc) (Integer/parseInt endc)]))

(defn count-range-filter [rangestr]
    (let [[start end] (rangestr-to-ints rangestr)]
      (fn [character password]
        (let [count-chars (count (re-seq (re-pattern character) password))]
          (and (<= start count-chars)
               (<= count-chars end))))))

(defn check-triplet-count [[rangestr characterplus password]]
  (let [testfn (count-range-filter rangestr)
        testchar (str (first characterplus))]
       (testfn testchar password)))

(defn parse-data [raw]
    (map #(clojure.string/split % #"\s") (clojure.string/split raw #"\n")))

(defmethod ifaces/run-problem "day2-2020-1" [x y]
  (let [triplet-vec (parse-data y)]
    (count (filter check-triplet-count triplet-vec))))
