(ns matlang.core-test
  (:require [clojure.test :refer :all]
            [matlang.core :refer :all]))

(deftest matrix-tests
  (is (= (list (list 1 1) (list 1 1)) ((comp :mat :val) ((->> "[1 1 ; 1 1];" parser interpreter)))))
  (is (= (list (list 1 1)) ((comp :mat :val) ((->> "[1 1];" parser interpreter))))))

(deftest addition
  ;test simple scalar addition
  (is (= {:type :scalar :val 0} ((->> "0 + 0;" parser interpreter))))
  (is (= {:type :scalar :val 99} ((->> "100 + -1;" parser interpreter))))
  ;test scalar matrix addition (broadcasting)
  (is (= (list (list 2 2) (list 2 2)) ((comp :mat :val) ((->> "1 + [1 1; 1 1];" parser interpreter)))))
  (is (= (list (list 2 2) (list 2 2)) ((comp :mat :val) ((->> "[1 1; 1 1] + 1;" parser interpreter)))))
  (is (= (list (list 1 1) (list 1 1)) ((comp :mat :val) ((->> "[0 0; 0 0] + [1 1; 1 1];" parser interpreter))))))
