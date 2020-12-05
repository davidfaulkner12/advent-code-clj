(ns advent-code.problem.day4-2020-1-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.problem.day4-2020-1 :refer :all]))

(deftest examples
  (are [x y] (= x (valid-passport? (parse-passport-to-map y)))
       true "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
       false "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"
       true "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm"
       false "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"))

(deftest actual-problem
  (is (= 264
         (ifaces/run-problem "day4-2020-1" (slurp "resources/2020-day4.input")))))
