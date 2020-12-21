(ns advent-code.problem.day16-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn parse-rules [raw-rules]
  (let [parsed-seq (re-seq #"([^:]+): (\d+)-(\d+) or (\d+)-(\d+)\n?" raw-rules)]
    (into {}
          (map
            (fn [[_ name n1 n2 n3 n4]]
              {name [[(Integer/parseInt n1)
                      (Integer/parseInt n2)]
                     [(Integer/parseInt n3)
                      (Integer/parseInt n4)]]})
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
    (apply +
      (filter (complement (partial valid-number? flat-rules)) flat-tix))))

(defmethod ifaces/run-problem ["day16-2020" "1"] [x y z]
  (let [[rules _ tickets] (parse-data z)]
    (calculate-error-rate rules tickets)))

; Can be used for flat rules of course
(defn valid-for-rule? [rules ticket]
  (empty? (filter (complement (partial valid-number? rules)) ticket)))

(defn column-values [valid-tickets]
  (apply (partial map vector) valid-tickets))

(defn which-rules-for-column [rules column]
  (map first (filter (fn [[k v]] (valid-for-rule? v column))
                     rules)))

(defn which-cols-for-rule [rule cols]
  (filter #(valid-for-rule? rule (nth cols %))
          (range (count cols))))

(defn shakeout [rules-to-cols shaken]
  (let [rules-to-cols-count (into {} (map (fn [[k v]] {k (count v)}) rules-to-cols))
        single-col-rules (filter (fn [[k v]] (and (not (contains? shaken k)) (= v 1))) rules-to-cols-count)]
    (if (empty? single-col-rules)
      rules-to-cols
      (let [[rule _] (first single-col-rules)
            col (first (rules-to-cols rule))
            new-rules-to-cols (into {} (map (fn [[k v]] (if (= k rule) {k v} {k (disj v col)})) rules-to-cols))]
        (shakeout new-rules-to-cols (conj shaken rule))))))

(defn find-rules-from-tickets [rules tickets]
  (let [cols (column-values tickets)
        rules-to-cols (into {} (map (fn [[k v]] {k (set (which-cols-for-rule v cols))}) rules))
        shakeout-rules-to-cols (shakeout rules-to-cols #{})]
    (set/map-invert (into {} (map (fn [[k v]] {k (first v)}) shakeout-rules-to-cols)))))

(defmethod ifaces/run-problem ["day16-2020" "2"] [x y z]
  (let [[rules ticket tickets] (parse-data z)
        flat-rules (deconstruct-rules rules)
        valid-tickets (filter (partial valid-for-rule? flat-rules) tickets)
        rules-to-col (find-rules-from-tickets rules valid-tickets)
        departure-cols (map first (filter (fn [[_ v]] (clojure.string/starts-with? v "departure")) rules-to-col))
        problem-output (apply * (map #(nth ticket %) departure-cols))]
    problem-output))
