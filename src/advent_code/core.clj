(ns advent-code.core
  (:require  [advent-code.interfaces :as ifaces])
  (:gen-class))

(defn problem-ns-symbol [problem-name]
  (symbol (str "advent-code.problem." problem-name)))

(defn load-problem-ns [problem-name]
  (require (problem-ns-symbol problem-name) :reload))

(defn -main
  [day problem file]
  (do
    (load-problem-ns day)
    (println
       (ifaces/run-problem day
                           problem
                           (slurp file)))))
