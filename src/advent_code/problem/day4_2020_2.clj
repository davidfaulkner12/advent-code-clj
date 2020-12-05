(ns advent-code.problem.day4-2020-2
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day4-2020-1 :as parent]))

(defn make-valid-year [start end]
  (fn [year]
    (and (some? (re-matches #"\d{4}" year))
         (<= start (Integer/parseInt year) end))))

(def valid-byr? (make-valid-year 1920 2002))

(def valid-iyr? (make-valid-year 2010 2020))

(def valid-eyr? (make-valid-year 2020 2030))

(defn valid-pid? [pid] (some? (re-matches #"\d{9}" pid)))

(defn valid-ecl? [ecl] (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl))

(defn valid-hcl? [hcl] (some? (re-matches #"#[a-f\d]{6}" hcl)))

(defn valid-hgt? [hgt]
  (let [[matches digits units] (re-matches #"(\d+)(cm|in)" hgt)]
    (cond (= "in" units) (<= 59 (Integer/parseInt digits) 76)
          (= "cm" units) (<= 150 (Integer/parseInt digits) 193)
          :else false)))

(defn really-valid-passport? [passport-map]
  (and (parent/valid-passport? passport-map)
       (valid-byr? (passport-map "byr"))
       (valid-iyr? (passport-map "iyr"))
       (valid-eyr? (passport-map "eyr"))
       (valid-pid? (passport-map "pid"))
       (valid-ecl? (passport-map "ecl"))
       (valid-hcl? (passport-map "hcl"))
       (valid-hgt? (passport-map "hgt"))))

(defmethod ifaces/run-problem "day4-2020-2" [x y]
  (count (filter really-valid-passport? (map parent/parse-passport-to-map (parent/split-data y)))))
