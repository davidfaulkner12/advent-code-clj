(ns advent-code.problem.day19-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn make-leaf-consume [test-char]
  (fn [char-seq]
    (if (= (first char-seq) test-char)
        (rest char-seq)
        nil)))

(defn make-compose [fns]
  (apply comp (reverse fns)))

(defn make-or-fn [fns]
  (fn [char-seq]
    (first (filter some? (map #(% char-seq) fns)))))

(defn create-leaf [index charstr]
  {(Integer/parseInt index) {:type :leaf :char (.charAt charstr 0)}})

(defn create-or [index or-string]
  (let [or-lines (clojure.string/split or-string #"\|")]
    {(Integer/parseInt index) {:type :or :list (map #(dh/to-edn-vec %) or-lines)}}))

(defn parse-line [raw-line]
  (let [[leaf-match leaf-index charstr] (re-matches #"(\d+): \"(\w)\"" raw-line)
        [or-match or-index or-string] (re-matches #"(\d+): ([\d ]+\|[\d ]+)" raw-line)
        [list-match list-index list-string] (re-matches #"(\d+): ([\d ]+)" raw-line)]
    (cond leaf-match (create-leaf leaf-index charstr)
          or-match (create-or or-index or-string)
          list-match {(Integer/parseInt list-index) {:type :rules :list (dh/to-edn-vec list-string)}}
          :else (println "Couldn't parse line:" raw-line))))


(defn build-tree [raw-data]
  (let [split-data (dh/split-lines raw-data)]
    (into {} (map parse-line split-data))))

(declare make-function-for-index)

(defn make-compose-fn-from-list [tree list]
  (make-compose (map (partial make-function-for-index tree) list)))

(defn make-function-for-index [tree index]
  (let [node (tree index)]
    (case (node :type)
      :leaf (make-leaf-consume (node :char))
      :rules (make-compose-fn-from-list tree (node :list))
      :or (make-or-fn (map (partial make-compose-fn-from-list tree) (node :list))))))

(defmethod ifaces/run-problem ["day19-2020" "1"] [x y z]
  (let [[raw-rules raw-test-lines] (dh/split-paragraphs z)
        rule-tree (build-tree raw-rules)
        zero-fn (make-function-for-index rule-tree 0)
        test-lines (map seq (dh/split-lines raw-test-lines))]
    (count (filter #(= '() %) (map zero-fn test-lines)))))

(defn validate-line [rule-tree test-line]
  (let [n42-fn (make-function-for-index rule-tree 42)
        n31-fn (make-function-for-index rule-tree 31)
        n8-fns (iterate #(comp n42-fn %) n42-fn)
        make-11-fn #(make-compose [n42-fn % n31-fn])
        n11-fns (rest (iterate make-11-fn identity))
        n8-outcomes (take-while some? (map #(% test-line) n8-fns))
        n11-outcomes (filter some? (for [x n8-outcomes y (take 5 n11-fns)]
                                     (y x)))]
    (first (drop-while (partial not= '() ) n11-outcomes))))

(defmethod ifaces/run-problem ["day19-2020" "2"] [x y z]
  (let [[raw-rules raw-test-lines] (dh/split-paragraphs z)
        rule-tree (build-tree raw-rules)
        test-lines (map seq (dh/split-lines raw-test-lines))]
    (count (filter #(= '() %) (map (partial validate-line rule-tree) test-lines)))))
