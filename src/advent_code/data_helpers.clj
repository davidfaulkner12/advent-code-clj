(ns advent-code.data-helpers
  (:require [clojure.edn :as edn]))

(def to-edn edn/read-string)

(defn read-string-test []
  (edn/read-string "{:foo \"bar\", :bar \"foo\"}"))

(defn wrap-vec [arg]
  (str "[" arg "]"))

(defn to-edn-vec [arg]
  (to-edn (wrap-vec arg)))
