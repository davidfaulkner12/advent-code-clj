(ns advent-code.data-helpers
  (:require [clojure.edn :as edn]))

(def to-edn edn/read-string)

(defn read-string-test []
  (edn/read-string "{:foo \"bar\", :bar \"foo\"}"))

(defn wrap-vec [arg]
  (str "[" arg "]"))

(defn wrap-map [arg]
  (str "{" arg "}"))

(defn to-edn-vec [arg]
  (to-edn (wrap-vec arg)))

(defn to-edn-map [arg]
  (to-edn (wrap-map arg)))

(defn split-lines [data]
  (clojure.string/split data #"\n"))

(defn split-paragraphs [data]
  (clojure.string/split data #"\n\n"))
