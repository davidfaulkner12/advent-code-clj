(ns advent-code.core
  (:require  [advent-code.interfaces :as ifaces])
  (:gen-class))

(defn -main
  [& args]
  (do
    (require (symbol (str "advent-code.problem." (first args))))
    (println
       (ifaces/run-problem (first args)
                           (slurp (second args))))))
