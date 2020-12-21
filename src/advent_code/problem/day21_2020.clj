(ns advent-code.problem.day21-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn parse-data [raw]
  (let [split-lines (re-seq #"(.*) \(contains (.*)\)\n" raw)
        ing-all (map (fn [[_ i a]] [(set (clojure.string/split i #"\s+")) (set (clojure.string/split a #"[ ,]+"))])
                     split-lines)]
    ing-all))

(defn get-allergy-to-possible-ing [ing-all]
  (apply (partial merge-with set/intersection)
         (for [[ings alls] ing-all
               a alls]
           {a ings})))

(defn bad-ingredients [ing-all]
  (apply set/union (vals (get-allergy-to-possible-ing ing-all))))

(defn all-ingredients [ing-all]
  (apply set/union (map first ing-all)))

(defn safe-ingredients [ing-all]
  (let [all-ings (all-ingredients ing-all)]
    (set/difference all-ings (bad-ingredients ing-all))))

(defmethod ifaces/run-problem ["day21-2020" "1"] [x y z]
  (let [parsed-data (parse-data z)
        safe-ings (safe-ingredients parsed-data)
        count-list (map #(count (set/intersection safe-ings (first %))) parsed-data)]
    (apply + count-list)))

(defn shakeout-bad-ingredients [bad-ings]
  (loop [ing-set bad-ings processed-set #{}]
    (if (= (count ing-set) (count processed-set))
        ing-set
        (let [[ing allset] (first (filter (fn [[k v]] (and (not (contains? processed-set k)) (= (count v) 1)))
                                        ing-set))
              all (first allset)
              new-ing-set (into {} (map (fn [[k v]] (if (= k ing) {k v} {k (disj v all)}))
                                        ing-set))]
          (recur new-ing-set (conj processed-set ing))))))

(defn sort-shaken-list [all-to-ings]
  (map #(first (second %)) (sort-by first all-to-ings)))

(defmethod ifaces/run-problem ["day21-2020" "2"] [x y z]
  (let [parsed-data (parse-data z)
        shaken-map (shakeout-bad-ingredients (get-allergy-to-possible-ing parsed-data))
        sorted-ings (sort-shaken-list shaken-map)]
    (clojure.string/join "," sorted-ings)))
