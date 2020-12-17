(ns advent-code.problem.day12-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day12-2020 :refer :all]))


(def example-1 (parse-data "F10
N3
F7
R90
F11
"))

(deftest test-forward-east
  (is (= ["E" 10 0]
         (forward ["E" 0 0] 10))))

(deftest test-forward-west
  (is (= ["W" -10 0]
         (forward ["W" 0 0] 10))))

(deftest test-forward-south
  (is (= ["S" 0 -10]
         (forward ["S" 0 0] 10))))

(deftest test-forward-north
 (is (= ["N" 0 10]
        (forward ["N" 0 0] 10))))

(deftest test-forward-cmd
  (is (= ["E" 10 0])
      (run-command ["E" 0 0] ["F" 10])))

(deftest test-rotate-right-cmd
  (is (= ["S" 0 0]
         (run-command ["E" 0 0] ["R" 90]))))

(deftest test-rotate-left-cmd
  (is (= ["W" 0 0]
         (run-command ["E" 0 0] ["L" 180]))))

(deftest test-dir-cmd
  (is (= ["E" 0 10]
         (run-command ["E" 0 0] ["N" 10]))))

(deftest test-example-cmds
  (is (= ["S" 17 -8]
         (run-command-list run-command ["E" 0 0] example-1))))

(deftest test-manhattan-distance
  (is (= 25
         (manhattan-distance-ship ["S" 17 -8]))))

(deftest actual-problem-1
  (is (= 1956
         (ifaces/run-problem "day12-2020" "1" (slurp "resources/2020-day12.input")))))

(deftest test-wp-fwd-cmd
  (is (= [100 10 10 1]
         (run-command-waypoint [0 0 10 1] ["F" 10]))))

(deftest test-wp-fwd-cmd-2
  (is (= [170 38 10 4]
         (run-command-waypoint [100 10 10 4] ["F" 7]))))

(deftest test-wp-dir-cmd
  (is (= [100 10 10 4]
         (run-command-waypoint [100 10 10 1] ["N" 3]))))

(deftest rotate-waypoint-cmd-180
  (is (= [100 10 10 4]
         (run-command-waypoint [100 10 -10 -4] ["R" 180])
         (run-command-waypoint [100 10 -10 -4] ["L" 180]))))

(deftest rotate-waypoint-cmd-90
  (is (= [100 10 4 -10]
         (run-command-waypoint [100 10 10 4] ["R" 90])
         (run-command-waypoint [100 10 10 4] ["L" 270]))))

(deftest test-example-wp
  (is (= [214 -72 4 -10]
         (run-command-list run-command-waypoint [0 0 10 1] example-1))))

(deftest actual-problem-2
  (is (= 126797
         (ifaces/run-problem "day12-2020" "2" (slurp "resources/2020-day12.input")))))
