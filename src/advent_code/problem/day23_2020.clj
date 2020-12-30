(ns advent-code.problem.day23-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn parse-data [raw]
  (filter #(<= 0 % 9) (map #(Character/getNumericValue %) (seq raw))))

(defn round [cups]
  (let [max-label (+ (count cups) 1)
        [cur pick1 pick2 pick3 & remaining] cups
        dest (first (filter #(not (contains? (set (list cur pick1 pick2 pick3 0)) %))
                            (iterate #(mod (dec %) max-label) cur)))
        [lhs rhs] (split-with (partial not= dest) remaining)]
    (concat lhs
            (list dest pick1 pick2 pick3)
            (rest rhs)
            (list cur))))

(defmethod ifaces/run-problem ["day23-2020" "1"] [x y z]
  (let [data (parse-data z)
        output (nth (iterate round data) 100)
        [lhs rhs] (split-with (partial not= 1) output)]
    (clojure.string/join "" (concat (rest rhs) lhs))))

(defn parse-data-map [data-vec]
  (let [curless-map (into {}
                      (for [[x y] (partition 2 1 data-vec)]
                        {x y}))]
    (assoc curless-map :cur (first data-vec)
                       (last data-vec) (first data-vec))))

(defn round-map [cups]
  (let [max-label (count cups) ; No need to increment here as we have the "extra" :cur key
        cur (cups :cur)
        pick1 (cups cur)
        pick2 (cups pick1)
        pick3 (cups pick2)
        next (cups pick3)
        dest (first (filter #(not (contains? (set (list cur pick1 pick2 pick3 0)) %))
                            (iterate #(mod (dec %) max-label) cur)))
        after-dest (cups dest)]
    ;(println "Cur" cur "Picks" pick1 pick2 pick3 "Next" next "Dest" dest "After-dest" after-dest)
    (assoc! cups
      cur next ; Pick up 3 cups, so after cur is now next
      dest pick1 ; Move the cups, so now dest's successor is our first cup ...
      pick3 after-dest ; And pick3's successor is whatever used to be after dest
      :cur next)))

(defn run-problem-1-map [raw]
  (let [data (transient (parse-data-map (parse-data raw)))
        output (nth (iterate round-map data) 100)
        output-list (take-while (partial not= 1) (iterate (partial output) (output 1)))]
    (clojure.string/join "" output-list)))

(defmethod ifaces/run-problem ["day23-2020" "2"] [x y z]
  (let [data-vec (parse-data z)
        million-list (take-while (partial >= 1000000) (iterate inc (+ (count data-vec) 1)))
        data (transient (parse-data-map (concat data-vec million-list)))
        output (nth (iterate round-map data) 10000000)
        a (output 1)
        b (output a)]
    (* a b)))
