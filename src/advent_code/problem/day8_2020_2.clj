(ns advent-code.problem.day8-2020-2
  (:require [clojure.set :as set]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day8-2020-1 :as parent]))

(defn index-all-nops-jmps [instructions]
  (first
    (reduce (fn [[jmp-nop-indices count] instruction]
              (if (or (clojure.string/starts-with? instruction "jmp")
                      (clojure.string/starts-with? instruction "nop"))
                  [(conj jmp-nop-indices count) (+ count 1)]
                  [jmp-nop-indices (+ count 1)]))
            [#{} 0]
            instructions)))

(defn switch-instruction [instruction]
  (if (clojure.string/starts-with? instruction "jmp")
      (clojure.string/replace instruction "jmp" "nop")
      (clojure.string/replace instruction "nop" "jmp")))

(defn alternate-single-instruction [instructions count]
  (assoc instructions count (switch-instruction (nth instructions count))))

(defn find-instruction [instructions]
  (let [jmp-nop-instructions (index-all-nops-jmps instructions)]
    (filter second (map #(parent/run-instructions (alternate-single-instruction instructions %)) (index-all-nops-jmps instructions)))))

(defmethod ifaces/run-problem "day8-2020-2" [x y]
  (first (first (find-instruction (parent/make-instructions y)))))
