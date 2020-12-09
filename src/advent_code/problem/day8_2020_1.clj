(ns advent-code.problem.day8-2020-1
  (:require [clojure.set :as set]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]))

(defn make-instructions [raw]
  (dh/split-lines raw))

(defn nop [ip acc arg]
  [(+ ip 1) acc])

(defn acc [ip acc arg]
  [(+ ip 1) (+ acc arg)])

(defn jmp [ip acc arg]
  [(+ ip arg) acc])

(def handlers {"nop" nop "acc" acc "jmp" jmp})

(defn run-instructions [instructions]
  (loop [ip 0 acc 0 ips #{}]
    (cond (contains? ips ip) [acc false]
          (>= ip (count instructions)) [acc true]
          :else (let [instruction (nth instructions ip)
                      [_ op argstr] (re-matches #"(\w+) (.*)" instruction)
                      opfn (handlers op)
                      arg (Integer/parseInt argstr)
                      [new-ip new-acc] (opfn ip acc arg)]
                  (recur new-ip new-acc (conj ips ip))))))

(defmethod ifaces/run-problem "day8-2020-1" [x y]
  (first (run-instructions (make-instructions y))))
