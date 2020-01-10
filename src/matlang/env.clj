(ns matlang.env)

(def env (atom '()))

(defmacro deref-top
  []
  `(deref (first @env)))

(defmacro get-ret
  []
  `(:_ret ~(deref-top)))

(defn push-env
  "push env on top of env stack"
  []
  (swap! env conj (atom {})))

(defn pop-env
  "pop env of the top of the stack, and returns ret"
  []
  (if (= 0 (count @env))
    (throw (Exception. "Empty environement stack"))
    (let [ret (get-ret)]
      (swap! env rest)
      ret)))

(defn init-env
  "create env stack from initial environment"
  [init-state]
  (if (= 0 (count @env))
    (swap! env conj (atom init-state))
    (throw (Exception. "Env can only be initialized once"))))


;;TODO: fix this methods to work woith global environement 


(defn get-env-value
  "get value from env"
  [env key]
  (loop [cur-env (first env) tail (rest env)]
    (if cur-env
      (let [cur-env @cur-env
            value (key cur-env)]
        (if (some? value)
          (if (= value :uninitialized)
            (throw (Exception. (str "Error: Uninitialized variable: " (name key))))
            value)
          (recur (first tail) (rest tail))))
      nil)))

(defn declare-env-value
  "declare a variable in the current environement"
  ([env key] (declare-env-value env key :uninitialized))
  ([env key value]
   (let [head (first env)]
     (if head
       (swap! head assoc key value)))))

(defn set-env-value
  "set value from env key"
  [env key value]
  (loop [cur-env (first env) tail (rest env)]
    (if cur-env
      (let [cur @cur-env]
        (if (key cur)
          (swap! cur-env assoc key value)
          (recur (first tail) (rest tail))))
      (throw (Exception. (str "Error: Undefined variable: " key))))))
