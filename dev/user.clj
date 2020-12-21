(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [advent-code.core :refer [load-problem-ns problem-ns-symbol]]
            [advent-code.data-helpers :as dh]
            [advent-code.data-helpers-test :as dh-test]
            [advent-code.interfaces-test :as ifaces-test]
            [clojure.test :refer [run-tests]]
            [advent-code.problem.day21-2020 :refer :all]
            [advent-code.problem.day21-2020-test :refer :all]))

(defn test-problem [problem]
  (let [problem-test (str problem "-test")]
    (load-problem-ns problem)
    (load-problem-ns problem-test)
    (run-tests (problem-ns-symbol problem-test))))
;
; (def data (slurp "resources/2020-day16.input"))
;
; (def parsed-data-1 (dh/split-paragraphs data))
; ;
(time (run-tests 'advent-code.problem.day21-2020-test))

; (def example-line (first (split-lines example)))
