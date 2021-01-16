(ns advent-code.problem.day25-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn parse-data [raw]
  (mapv #(Integer/parseInt %) (dh/split-lines raw)))

(def initial-subject 7)
(def prime 20201227)

(defn make-transform [subject-number]
  (fn [val]
    (rem (* val subject-number) prime)))

(def transform (make-transform 7))

(defn identify-secret-loop [public-key]
  (let [[secret-loop _] (first
                          (drop-while
                            (fn [[_ v]] (not= v public-key))
                            (map-indexed vector (iterate transform 1))))]
    secret-loop))

(defn calc-secret-key [door-public-key card-public-key]
  (let [card-secret-loop (identify-secret-loop card-public-key)
        door-secret-loop (identify-secret-loop door-public-key)]
    {:card-secret-key (nth (iterate (make-transform door-public-key) 1) card-secret-loop)
     :door-secret-key (nth (iterate (make-transform card-public-key) 1) door-secret-loop)}))

(defmethod ifaces/run-problem ["day25-2020" "1"] [x y z]
  (let [[door-key card-key] (parse-data z)]
    (:card-secret-key (calc-secret-key door-key card-key))))
