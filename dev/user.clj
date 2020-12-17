(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [advent-code.core :refer [load-problem-ns problem-ns-symbol]]
            [advent-code.data-helpers :as dh]
            [advent-code.data-helpers-test :as dh-test]
            [advent-code.interfaces-test :as ifaces-test]
            [clojure.test :refer [run-tests]]
            [advent-code.problem.day11-2020 :refer :all]
            [advent-code.problem.day11-2020-test :refer :all]))

(defn test-problem [problem]
  (let [problem-test (str problem "-test")]
    (load-problem-ns problem)
    (load-problem-ns problem-test)
    (run-tests (problem-ns-symbol problem-test))))

; (def data (slurp "resources/2020-day9.input"))
;
; (def parsed-data (dh/to-edn-vec data))
;
(time (run-tests 'advent-code.problem.day11-2020-test))

; (def example-line (first (split-lines example)))

(def example-range (for [x (range 0 10)
                         y (range 0 10)]
                     [x y]))
