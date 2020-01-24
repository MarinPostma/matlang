(ns matlang.env)

(defn first-env
  [env]
  (let [head (first @env)]
    (if head
      @head
      nil)))

(defn get-ret
  [env]
  (:_ret (first-env env)))

(defn push-env-fn
  "take an env stack and return a function that push an env on top of the env stack and returns the stack"
  [env]
  (fn [] (swap! env conj (atom {}))))

(defn pop-env-fn
  "takes an env stack and return a function that pops the env stack an return the ret value of the env"
  [env]
  (fn []
    (if (= 0 (count @env))
      (throw (Exception. "Empty environement stack"))
      (let [ret (get-ret env)]
        (swap! env rest)
        ret))))

(defn init-env
  "create env stack from initial environment"
  [init-state]
  (let [env (atom '())]
    (swap! env conj (atom init-state))
    env))

(defn get-env-val-fn
  "take an env stack and returns a function that gets a value in this env stack"
  [env]
  (fn [key]
    (let [env @env]
      (loop [cur-env (first env) tail (rest env)]
        (if cur-env
          (let [cur-env @cur-env
                value (key cur-env)]
            (if value
              (if (= value :uninitialized)
                (throw (Exception. (str "Error: Uninitialized variable: " (name key))))
                value)
              (recur (first tail) (rest tail))))
          nil)))))

(defn declare-env-value-fn
  "declare a variable in the current environment"
  [env]
  (fn [k]
    (let [head (first @env)]
      (if head
        (swap! head assoc k :uninitialized)))))

(defn set-env-value-fn
  "set value from env key"
  [env]
  (fn [key value]
    (loop [cur-env (first @env) tail (rest @env)]
      (if cur-env
        (let [cur @cur-env]
          (if (key cur)
            (swap! cur-env assoc key value)
            (recur (first tail) (rest tail))))
        (throw (Exception. (str "Error: Undefined variable: " key)))))))

(defmacro make-env-fn
  "generate environnement functions for a given env"
  [env]
  `(do
     (def ~(symbol "pop-env") (pop-env-fn ~env))
     (def ~(symbol "push-env") (push-env-fn ~env))
     (def ~(symbol "set-env-value") (set-env-value-fn ~env))
     (def ~(symbol "get-env-value") (get-env-val-fn ~env))
     (def ~(symbol "declare-env-value") (declare-env-value-fn ~env))))
