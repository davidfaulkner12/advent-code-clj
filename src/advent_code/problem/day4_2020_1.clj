(ns advent-code.problem.day4-2020-1
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]))

(defn split-data [raw]
  (clojure.string/split raw #"\n\n"))

(defn parse-passport-to-map [passport-str]
  (apply hash-map (clojure.string/split passport-str #"[\s:]+")))

(def valid-keys #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"})

(defn valid-passport? [passport-map]
  (let [passport-keys (set (keys passport-map))
        missing-keys (empty? (clojure.set/difference (disj valid-keys "cid") passport-keys))]
    missing-keys))

(defmethod ifaces/run-problem "day4-2020-1" [x y]
  (count (filter valid-passport? (map parse-passport-to-map (split-data y)))))
