(ns advent-code.problem.day2-2020-1-test
  (:require [clojure.test :refer :all]
            [advent-code.problem.day2-2020-1 :refer :all]))

(deftest split-lines
  (is (= (split-data "4-5 t: ftttttrvts\n7-8 k: kkkkkkkf\n")
         [["4-5 t: ftttttrvts\n" "4" "5" "t" "ftttttrvts"]
          ["7-8 k: kkkkkkkf\n" "7" "8" "k" "kkkkkkkf"]])))

(deftest single-line
  (is (= (parse-single-line ["4-5 t: ftttttrvts\n" "4" "5" "t" "ftttttrvts"])
         [4 5 \t "ftttttrvts"])))

(deftest examples
  (are [x y] (= ((check-line make-count-filter) x) y)
    [1 3 \a "abcde"] true
    [1 3 \b "cdefg"] false
    [2 9 \c "ccccccccc"] true))
