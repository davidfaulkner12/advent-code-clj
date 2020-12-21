(ns advent-code.problem.day21-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day21-2020 :refer :all]))

(def raw-example "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
")

(def example (parse-data raw-example))

(deftest test-safe-ingredients
  (is (= #{"nhms" "trh" "kfcds" "sbzzf"}
         (safe-ingredients example))))

(deftest actual-problem-1-example
  (is (= 5
         (ifaces/run-problem "day21-2020" "1" raw-example))))

(deftest actual-problem-1-real
  (is (= 2315
         (ifaces/run-problem "day21-2020" "1" (slurp "resources/2020-day21.input")))))

(deftest shakeout-example
  (is (= {"dairy" #{"mxmxvkd"}, "fish" #{"sqjhc"}, "soy" #{"fvjkl"}}
         (shakeout-bad-ingredients (get-allergy-to-possible-ing example)))))

(deftest sorted-shaken-map
  (is (= '("mxmxvkd" "sqjhc" "fvjkl")
         (sort-shaken-list {"fish" #{"sqjhc"}, "dairy" #{"mxmxvkd"}, "soy" #{"fvjkl"}}))))

(deftest actual-problem-2-example
  (is (= "mxmxvkd,sqjhc,fvjkl"
         (ifaces/run-problem "day21-2020" "2" raw-example))))

(deftest actual-problem-2-example
  (is (= "cfzdnz,htxsjf,ttbrlvd,bbbl,lmds,cbmjz,cmbcm,dvnbh"
         (ifaces/run-problem "day21-2020" "2" (slurp "resources/2020-day21.input")))))
