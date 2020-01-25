(ns matlang.env-test
  (:require
   [clojure.test :refer :all]
   [matlang.env :refer :all]))

;; TODO: update tests
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

(deftest test-push-env
  (let [env (init-env {:foo "bar"})
        push-env (push-env-fn env)]
    (is (= {:foo "bar"} (first-env env)))
    (push-env)
    (is (= {} (first-env env)))))

(deftest test-pop-env
  (let [env (init-env {:_ret "bar"})
        push-env (push-env-fn env)
        pop-env (pop-env-fn env)]
    (push-env)
    (is (= nil (pop-env)))
    (is (= "bar" (pop-env)))))

(deftest test-get-env-var
  (let [env (init-env {:foo "bar"})
        push-env (push-env-fn env)
        get-env-val (get-env-val-fn env)]
    (is (= "bar" (get-env-val :foo)))
    (is (= nil (get-env-val :bar)))
    (push-env)
    (swap! (first @env) assoc :bar "foobar" :foo "neofoo")
    (is (= "neofoo" (get-env-val :foo)))
    (is (= "foobar" (get-env-val :bar)))))

(deftest test-declare-env-val
  (let [env (init-env {})
        get-env-val (get-env-val-fn env)
        declare-env-value (declare-env-value-fn env)]
    (declare-env-value :hello)
    (is (thrown? Exception (get-env-val :hello)))))

(deftest test-declare-env-val
  (let [env (init-env {})
        get-env-val (get-env-val-fn env)
        set-env-val (set-env-value-fn env)
        declare-env-value (declare-env-value-fn env)]
    (is (thrown? Exception (set-env-val :hello 12)))
    (declare-env-value :hello)
    (set-env-val :hello 12)
    (is (= (get-env-val :hello) 12))))
