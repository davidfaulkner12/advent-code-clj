(ns advent-code.problem.day5-2020-1
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]))

(defn parse-data [raw]
  (map seq (dh/split-lines raw)))

(defn boarding-pass-reducer [boarding-pass]
  (reduce (fn [[minrow maxrow mincol maxcol] char]
              (cond (= char \F) [minrow (quot (+ minrow maxrow) 2) mincol maxcol]
                    (= char \B) [(+ (quot (+ minrow maxrow) 2) 1) maxrow mincol maxcol]
                    (= char \L) [minrow maxrow mincol (quot (+ mincol maxcol) 2)]
                    (= char \R) [minrow maxrow (+ (quot (+ mincol maxcol) 2) 1) maxcol]))
          [0 127 0 7]
          boarding-pass))

(defn rowcol-for-boarding-pass [boarding-pass]
  (let [reduction (boarding-pass-reducer boarding-pass)]
    [(nth reduction 0) (nth reduction 2)]))

(defn seat-id [[row col]]
  (+ (* row 8) col))

(defmethod ifaces/run-problem "day5-2020-1" [x y]
  (let [parsed-data (parse-data y)
        all-rows-and-cols (map rowcol-for-boarding-pass parsed-data)
        all-seat-ids (map seat-id all-rows-and-cols)]
    (apply max all-seat-ids)))
