(ns advent-code.problem.day22-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn parse-data [raw]
  (let [[raw-p1 raw-p2] (dh/split-paragraphs raw)
        p1 (mapv #(Integer/parseInt %) (rest (dh/split-lines raw-p1)))
        p2 (mapv #(Integer/parseInt %) (rest (dh/split-lines raw-p2)))]
    [(queue p1) (queue p2)]))

(defn play-combat [p1 p2]
  (loop [cur-p1 p1 cur-p2 p2]
    (if (or (empty? cur-p1) (empty? cur-p2))
        [(seq cur-p1) (seq cur-p2)]
        (let [[[c1 rest-p1] [c2 rest-p2]] (map (juxt peek pop) [cur-p1 cur-p2])]
          (cond (> c1 c2)
                (recur (conj rest-p1 c1 c2) rest-p2)
                (< c1 c2)
                (recur rest-p1 (conj rest-p2 c2 c1))
                :else
                "DIE DIE DIE TIE TIE TIE")))))

(defn calculate-score [cards]
  (first (reduce (fn [[score count] v]
                   [(+ score (* v count)) (dec count)])
                 [0 (count cards)]
                 cards)))

(defmethod ifaces/run-problem ["day22-2020" "1"] [x y z]
  (let [[p1 p2] (parse-data z)
        [out-p1 out-p2] (play-combat p1 p2)]
    (calculate-score (if (empty? out-p1) out-p2 out-p1))))

(defn play-rec-combat [p1 p2]
  (loop [cur-p1 p1 cur-p2 p2 seen-states #{}]
    (if (contains? seen-states [cur-p1 cur-p2])
      [p1 nil]
      (if (or (empty? cur-p1) (empty? cur-p2))
          [(seq cur-p1) (seq cur-p2)]
          (let [next-seen-states (conj seen-states [cur-p1 cur-p2])
                [[c1 rest-p1] [c2 rest-p2]] (map (juxt peek pop) [cur-p1 cur-p2])
                count-1 (count rest-p1)
                count-2 (count rest-p2)]
            (cond (and (<= c1 count-1) (<= c2 count-2))
                  (let [[rec-p1 rec-p2] (play-rec-combat (queue (take c1 rest-p1)) (queue (take c2 rest-p2)))]
                    (if (nil? rec-p1)
                        (recur rest-p1 (conj rest-p2 c2 c1) next-seen-states)
                        (recur (conj rest-p1 c1 c2) rest-p2 next-seen-states)))
                  (> c1 c2)
                  (recur (conj rest-p1 c1 c2) rest-p2 next-seen-states)
                  (< c1 c2)
                  (recur rest-p1 (conj rest-p2 c2 c1) next-seen-states)
                  :else
                  "DIE DIE DIE TIE TIE TIE"))))))

(defmethod ifaces/run-problem ["day22-2020" "2"] [x y z]
  (let [[p1 p2] (parse-data z)
        [out-p1 out-p2] (play-rec-combat p1 p2)]
    (calculate-score (if (empty? out-p1) out-p2 out-p1))))
