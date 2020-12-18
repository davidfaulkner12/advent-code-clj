(ns advent-code.problem.day8-2020
  (:require [advent-code.interfaces :as ifaces]
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

(defmethod ifaces/run-problem ["day8-2020" "1"] [x y z]
  (first (run-instructions (make-instructions z))))

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
    (filter second (map #(run-instructions (alternate-single-instruction instructions %)) (index-all-nops-jmps instructions)))))

(defmethod ifaces/run-problem ["day8-2020" "2"] [x y z]
  (first (first (find-instruction (make-instructions z)))))
