(ns advent-code.problem.day13-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]))

(defn parse-data [raw]
  (let [ints (map #(Integer/parseInt %) (filter #(not= "x" %) (clojure.string/split raw #"[\s,]")))]
    [(first ints) (rest ints)]))

(defn arrives? [time bus]
  (= 0 (rem time bus)))

(defn pick-arrives [time bus-sched]
  (filter (partial arrives? time) bus-sched))

; Elegant but can't get bus number :-)
; (defn next-bus [[time bus-sched]])
;   (first (drop-while #(empty? (pick-arrives % bus-sched))
;                      (iterate inc time))))

(defn next-bus [[time bus-sched]]
  (loop [cur-time time]
    (let [arrivals (pick-arrives cur-time bus-sched)]
      (if (seq arrivals)
          [cur-time (first arrivals)]
          (recur (inc cur-time))))))

(defmethod ifaces/run-problem ["day13-2020" "1"] [x y z]
  (let [data (parse-data z)
        [start-time _] data
        [end-time bus] (next-bus data)]
    (* bus (- end-time start-time))))

(defn parse-data-2 [raw]
  (let [txt-line (second (dh/split-lines raw))
        separated (vec (clojure.string/split txt-line #","))
        line (mapv (fn [x] (if (= x "x") 1 (Integer/parseInt x))) separated)]
    line))

(defn valid-schedule? [cur-time bus-sched]
  (every? identity
    (for [x (range (count bus-sched))]
      (arrives? (+ cur-time x) (nth bus-sched x)))))

(defn invert-schedule [bus-sched]
  (apply hash-map
    (flatten
      (for [x (range (count bus-sched))
            :let [bus (nth bus-sched x)]
            :when (not= 1 bus)]
          [bus (mod (- x) bus)]))))

(defn chinese-remainder [mod-map]
  (let [first-max-key (apply max (keys mod-map))]
    ; a and n from wikipedia
    (loop [cur-map (dissoc mod-map first-max-key) cur-n first-max-key cur-a (mod-map first-max-key)]
      (if (empty? cur-map)
          cur-a
          (let [max-key (apply max (keys cur-map))
                sequence (iterate (partial + cur-n) cur-a)
                next-a (first (drop-while #(not= (cur-map max-key) (mod % max-key)) sequence))]
            (recur (dissoc cur-map max-key) (* max-key cur-n) next-a))))))

(defmethod ifaces/run-problem ["day13-2020" "2"] [x y z]
  (let [data (parse-data-2 z)]
    (chinese-remainder (invert-schedule data))))
