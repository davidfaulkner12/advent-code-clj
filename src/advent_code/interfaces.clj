(ns advent-code.interfaces)

(defmulti string->data (fn [x y] x))
(defmulti run-problem (fn [x y] x))
