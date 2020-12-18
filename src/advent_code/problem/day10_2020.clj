(ns advent-code.problem.day10-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]))

(defn sequential-diffs [data]
  (let [sorted-data (sort (conj data 0))
        partitioned-data (partition 2 1 sorted-data)
        diffs (map (fn [[x y]] (Math/abs (- x y))) partitioned-data)]
    diffs))

(defn difference-count [data]
  (let [diffs (sequential-diffs data)]
    ; We always have a final 3
    (update (frequencies diffs) 3 inc)))

(defn multiply-counts [jolt-counts]
  (* (jolt-counts 3) (jolt-counts 1)))

(defmethod ifaces/run-problem ["day10-2020" "1"] [x y z]
  (multiply-counts (difference-count (dh/to-edn-vec z))))

(defn sequential-ones [diffs]
  (map count (filter #(= (first %) 1) (partition-by identity diffs))))

; Stolen from mega-thread https://dev.to/rpalo/comment/193c4
(def lookup-multiplier
  {1 1
   2 2
   3 4
   4 7})

(defmethod ifaces/run-problem ["day10-2020" "2"] [x y z]
  (apply * (map lookup-multiplier (sequential-ones (sequential-diffs (dh/to-edn-vec z))))))
