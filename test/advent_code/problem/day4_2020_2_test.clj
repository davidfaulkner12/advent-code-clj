(ns advent-code.problem.day4-2020-2-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.problem.day4-2020-1 :refer :all]
            [advent-code.problem.day4-2020-2 :refer :all]))

(deftest byr-examples
  (are [x y] (= x (valid-byr? y))
    false "abcd"
    true "2002"
    false "2003"))

(deftest pid-examples
  (are [x y] (= x (valid-pid? y))
    true "000000001"
    false "0123456789"))

(deftest ecl-examples
  (are [x y] (= x (valid-ecl? y))
    true "brn"
    false "wat"))

(deftest hcl-examples
  (are [x y] (= x (valid-hcl? y))
    true "#123abc"
    false "123abz"
    false "123abc"))

(deftest hgt-examples
  (are [x y] (= x (valid-hgt? y))
    true "60in"
    true "190cm"
    false "190in"
    false "190"))

(deftest really-valid-examples
  (are [x y] (= x (really-valid-passport? (parse-passport-to-map y)))
    false "eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
    false "iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946"
    false "hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
    false "hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007"
    true "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"
    true "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
    true "hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022"
    true "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"))


(deftest actual-problem
  (is (= 224
         (ifaces/run-problem "day4-2020-2" (slurp "resources/2020-day4.input")))))
