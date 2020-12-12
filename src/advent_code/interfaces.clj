(ns advent-code.interfaces)

(defmulti run-problem (fn [problem day data] [problem day]))
