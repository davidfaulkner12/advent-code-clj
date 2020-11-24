(ns advent-code.core
  (:require  [advent-code.interfaces :as ifaces])
  (:gen-class))

(defn problem-ns-symbol [problem-name]
  (symbol (str "advent-code.problem." problem-name)))

(defn load-problem-ns [problem-name]
  (require (problem-ns-symbol problem-name) :reload))

(defn -main
  [& args]
  (do
    (load-problem-ns (first args))
    (println
       (ifaces/run-problem (first args)
                           (slurp (second args))))))
