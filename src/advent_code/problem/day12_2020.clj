(ns advent-code.problem.day12-2020
  (:require [clojure.set :as set]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]))

(defn parse-data [raw]
  (map (fn [[_ cmd amt]] [cmd (Integer/parseInt amt)])
       (re-seq #"(\w)(\d+)\n" raw)))

(defn forward [[ship-dir ship-x ship-y] amt]
  (case ship-dir
        "E" [ship-dir (+ ship-x amt) ship-y]
        "W" [ship-dir (- ship-x amt) ship-y]
        "N" [ship-dir ship-x (+ ship-y amt)]
        "S" [ship-dir ship-x (- ship-y amt)]))

(def degrees-to-dir
  {0 "N"
   90  "E"
   180 "S"
   270 "W"})

(def dir-to-degrees
  (set/map-invert degrees-to-dir))

(defn rotate-ship [[ship-dir ship-x ship-y] [dir amt]]
  (let [dir-fn (if (= dir "R") + -)
        old-angle (dir-to-degrees ship-dir)
        new-angle (mod (dir-fn old-angle amt) 360)]
    [(degrees-to-dir new-angle) ship-x ship-y]))

(defn run-command [ship [cmd amt]]
  (cond (= cmd "F") (forward ship amt)
        (or (= cmd "R") (= cmd "L")) (rotate-ship ship [cmd amt])
        :else (let [[ship-dir ship-x ship-y] ship]
                (assoc (forward [cmd ship-x ship-y] amt) 0 ship-dir))))

(defn run-command-list [cmd-fn starting-ship cmd-list]
  (reduce cmd-fn starting-ship cmd-list))

(defn manhattan-distance [ship-x ship-y]
  (+ (Math/abs ship-x) (Math/abs ship-y)))

(defn manhattan-distance-ship [[_ ship-x ship-y]]
  (manhattan-distance ship-x ship-y))

(defmethod ifaces/run-problem ["day12-2020" "1"] [x y z]
  (manhattan-distance-ship (run-command-list run-command ["E" 0 0] (parse-data z))))

(defn forward-waypoint [[ship-x ship-y waypoint-x waypoint-y]]
  [(+ ship-x waypoint-x) (+ ship-y waypoint-y) waypoint-x waypoint-y])

(defn rotate-waypoint [[ship-x ship-y waypoint-x waypoint-y] [cmd amt]]
  (let [right-amt (if (= cmd "R") amt (mod (- amt) 360))]
    (cond (= right-amt 270) [ship-x ship-y (- waypoint-y) waypoint-x]
          (= right-amt 180) [ship-x ship-y (- waypoint-x) (- waypoint-y)]
          (= right-amt 90) [ship-x ship-y waypoint-y (- waypoint-x)])))

(defn run-command-waypoint [ship-waypoint [cmd amt]]
  (cond (= cmd "F") (last (take (+ amt 1) (iterate forward-waypoint ship-waypoint)))
        (or (= cmd "R") (= cmd "L")) (rotate-waypoint ship-waypoint [cmd amt])
        :else (let [[ship-x ship-y wp-x wp-y] ship-waypoint
                    [_ new-wp-x new-wp-y] (forward [cmd wp-x wp-y] amt)]
                [ship-x ship-y new-wp-x new-wp-y])))

(defn manhattan-distance-ship-wp [[ship-x ship-y]]
  (manhattan-distance ship-x ship-y))

(defmethod ifaces/run-problem ["day12-2020" "2"] [x y z]
  (manhattan-distance-ship-wp (run-command-list run-command-waypoint [0 0 10 1] (parse-data z))))
