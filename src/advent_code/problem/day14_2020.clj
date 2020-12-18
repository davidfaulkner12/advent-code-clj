(ns advent-code.problem.day14-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn parse-mem-line [lhs rhs]
  (let [addr (re-find #"\d+" lhs)]
    [(Integer/parseInt addr) (Integer/parseInt rhs)]))

(defn parse-data [raw]
  (map (fn [[_ lhs rhs]]
         (if (= lhs "mask")
             [lhs rhs]
             (parse-mem-line lhs rhs)))
    (re-seq #"(.*) = (.*)\n" raw)))

(defn to-36bits [num]
  (let [bitstring (Integer/toBinaryString num)
        padding (apply str (repeat (- 36 (count bitstring)) "0"))]
    (str padding bitstring)))

(defn create-bitmask [maskstr]
  (reduce (fn [map index]
            (if (= \X (nth maskstr index))
                map
                (assoc map index (nth maskstr index))))
          {}
          (range 0 (count maskstr))))

(defn mask-bits [number bitmask]
  (Long/parseLong (apply str (let [number-str (vec (to-36bits number))]
                               (reduce (fn [n [k v]] (assoc n k v))
                                       number-str
                                       bitmask)))
                  2))


(defn run-program [cmds]
  (first (reduce (fn [[state cur-bitmask] [cmd val]]
                   (if (= cmd "mask")
                     [state (create-bitmask val)]
                     (let [masked-val (mask-bits val cur-bitmask)]
                       [(assoc state cmd masked-val) cur-bitmask])))
                 [{} {}]
                 cmds)))

(defmethod ifaces/run-problem ["day14-2020" "1"] [x y z]
  (apply + (vals (run-program (parse-data z)))))

(defn create-bitmask-map-2 [maskstr]
  (reduce (fn [map index]
            (if (= \0 (nth maskstr index))
                map
                (assoc map index (nth maskstr index))))
          {}
          (range 0 (count maskstr))))

(defn powerset [items]
  (reduce
    (fn [s x]
      (set/union s (map #(conj % x) s)))
    (hash-set #{})
    items))

(defn make-permutations [xs]
  (let [power-xs (powerset xs)
        zero-map (into {} (for [x xs] {x \0}))]
    (for [power-x power-xs]
      (merge zero-map (into {} (for [x power-x] {x \1}))))))

(defn permute-map [bitmask-map]
  (let [xs (set (map first (get (group-by val bitmask-map) \X)))
        permutations (make-permutations xs)]
    (for [x permutations]
        (merge bitmask-map x))))

(defn make-write-function [bitmask-set]
  (fn [mem addr val]
    (reduce (fn [cur-state cur-bitmask]
              (let [masked-addr (mask-bits addr cur-bitmask)]
                (assoc cur-state masked-addr val)))
            mem
            bitmask-set)))

(defn run-program-2 [cmds]
  (first (reduce (fn [[state cur-bitmask-fn] [cmd val]]
                   (if (= cmd "mask")
                     [state (make-write-function (permute-map (create-bitmask-map-2 val)))]
                     [(cur-bitmask-fn state cmd val) cur-bitmask-fn]))
                 [{} identity]
                 cmds)))

(defmethod ifaces/run-problem ["day14-2020" "2"] [x y z]
  (apply + (vals (run-program-2 (parse-data z)))))
