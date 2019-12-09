(ns matlang.core-test
  (:require [clojure.test :refer :all]
            [matlang.core :refer :all]))

(deftest addition
  ;test simple scalar addition
  (is (= {:type :scalar :val 0} ((->> "0 + 0;" parser interpreter))))
  (is (= {:type :scalar :val 99} ((->> "100 + -1;" parser interpreter))))
  ;test scalar matrix addition (broadcasting)
  (is (= {:type :matricx :val {:shape `(2 2) :mat (`(1 1) `(1 1))}} ((->> "[0 0; 0 0] + [1 1; 1 1];" parser interpreter)))))
