(ns advent-code.interfaces)

(defmulti string->data (fn [x y] x)) ; added but not used yet
(defmulti run-problem (fn [problem data] problem))
