(ns matlang.env-test
  (:require
   [clojure.test :refer :all]
   [matlang.env :refer :all]))

(deftest test-env-init
  (let [env (init-env {})
        env-deref @env
        head (deref (first env-deref))]
    (is (= {} head)))
  (let [env (init-env {:foo "bar"})
        env-deref @env
        head (deref (first env-deref))]
    (is (= {:foo "bar"} head))))

(deftest test-first-env
  (let [env (init-env {:foo "bar"})
        empty-env (atom '())]
    (is (= nil (first-env empty-env)))
    (is (= {:foo "bar"} (first-env env)))))

(deftest test-get-ret
  (let [env-ret (init-env {:_ret "hello"})
        env-no-ret (init-env {:foo "bar"})]
    (is (= "hello" (get-ret env-ret)))
    (is (= nil (get-ret env-no-ret)))))
