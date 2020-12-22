(ns advent-code.problem.day18-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn parse-line [linestr]
  (let [replace-values (clojure.string/replace linestr #"\+|\*"
                                              {"*" "\\*" "+" "\\+"})]
    (dh/to-edn-vec replace-values)))

(defn execute-line [ops]
  (first (reduce (fn [[cur-lhs cur-op] next-op]
                    (let [next (if (seq? next-op)
                                   (execute-line next-op)
                                   next-op)]
                      (cond (nil? cur-lhs)
                            [next cur-op]
                            (and (int? cur-lhs) (int? next))
                            [(cur-op cur-lhs next) nil]
                            (= next \*)
                            [cur-lhs *]
                            (= next \+)
                            [cur-lhs +])))
                 [nil nil]
                 ops)))

(defmethod ifaces/run-problem ["day18-2020" "1"] [x y z]
  (let [all-problems (map parse-line (dh/split-lines z))
        all-solutions (map execute-line all-problems)]
    (apply + all-solutions)))


(defn parse-line-2 [linestr]
  (let [replace-values (clojure.string/replace linestr #"\(|\)|\+|\*"
                                              {"*" "\\*" "+" "\\+" "(" "\\( " ")" "\\) "})]
    (dh/to-edn-vec replace-values)))

(defn pop-plus [output op-stack]
  (let [next-op (peek op-stack)]
    (if (= next-op \+)
      (pop-plus (conj output \+) (pop op-stack))
      [output (conj op-stack \*)])))

(defn pop-paren [output op-stack]
  (let [next-op (peek op-stack)]
    (if (= next-op \()
      [output (pop op-stack)]
      (pop-paren (conj output next-op) (pop op-stack)))))


(defn first-pass [ops]
  (reduce (fn [[output op-stack] next-op]
            (cond (int? next-op)
                  [(conj output next-op) op-stack]
                  (or (= next-op \+) (= next-op \())
                  [output (conj op-stack next-op)]
                  (= next-op \*)
                  (pop-plus output op-stack)
                  (= next-op \))
                  (pop-paren output op-stack)))
          [(queue) []]
          ops))

(defn process-tokens [ops]
  (let [[output op-stack] (first-pass ops)]
    (seq (reduce conj output (reverse op-stack)))))

(defn pop-with-op [stack next-op]
  (let [real-op (if (= next-op \*) * +)
        n1 (peek stack)
        n2 (peek (pop stack))]
    (conj (pop (pop stack)) (real-op n1 n2))))


(defn calculate-rpn [ops]
  (peek (reduce (fn [stack next-op]
                  (if (int? next-op)
                      (conj stack next-op)
                      (pop-with-op stack next-op)))
                []
                ops)))

(defmethod ifaces/run-problem ["day18-2020" "2"] [x y z]
  (let [all-solutions (map (comp calculate-rpn process-tokens parse-line-2) (dh/split-lines z))]
    (apply + all-solutions)))
