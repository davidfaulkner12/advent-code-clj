(ns advent-code.problem.day16-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn parse-rules [raw-rules]
  (let [parsed-seq (re-seq #"([^:]+): (\d+)-(\d+) or (\d+)-(\d+)\n?" raw-rules)]
    (into {}
          (map
            (fn [[_ name n1 n2 n3 n4]]
              {name [(Integer/parseInt n1)
                     (Integer/parseInt n2)
                     (Integer/parseInt n3)
                     (Integer/parseInt n4)]})
            parsed-seq))))

(defn parse-data [raw]
  (let [[rules ticket nearby-tickets] (dh/split-paragraphs raw)
        ticket (dh/to-edn-vec (second (dh/split-lines ticket)))
        nearby-tickets (map dh/to-edn-vec (rest (dh/split-lines nearby-tickets)))]
    [(parse-rules rules) ticket nearby-tickets]))

(defn deconstruct-rules [rules-map]
  (partition 2 (flatten (vals rules-map))))

(defn valid-number? [flat-rules number]
  (some (fn [[start end]] (<= start number end)) flat-rules))

(defn calculate-error-rate [rules tickets]
  (let [flat-rules (deconstruct-rules rules)
        flat-tix (flatten tickets)]
    (println flat-rules flat-tix)
    (apply +
      (filter (complement (partial valid-number? flat-rules)) flat-tix))))

(defmethod ifaces/run-problem ["day16-2020" "1"] [x y z]
  (let [[rules _ tickets] (parse-data z)]
    (calculate-error-rate rules tickets)))
