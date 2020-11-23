(ns advent-code.data-helpers-test
  (:require [clojure.test :refer :all]
            [advent-code.data-helpers :refer :all]))

(deftest wrap-vec-test-spaces
  (is (= [1 2 3]
         (-> "1 2 3"
             (wrap-vec)
             (to-edn)))))

(deftest wrap-vec-test-newlines
  (is (= [1 2 3]
         (-> "1\n2\n3"
             (wrap-vec)
             (to-edn)))))

(deftest wrap-vec-test-commas
  (is (= [1 2 3]
         (-> "1,2,3"
             (wrap-vec)
             (to-edn)))))
